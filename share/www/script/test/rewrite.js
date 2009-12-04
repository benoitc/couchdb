// Licensed under the Apache License, Version 2.0 (the "License"); you may not
// use this file except in compliance with the License. You may obtain a copy of
// the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
// WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
// License for the specific language governing permissions and limitations under
// the License.



couchTests.rewrite = function(debug) {
  // this test _rewrite handler
  
  
  var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
  db.deleteDb();
  db.createDb();
  
  
  if (debug) debugger;
  run_on_modified_server(
    [{section: "httpd",
      key: "authentication_handlers",
      value: "{couch_httpd_auth, special_test_authentication_handler}"},
     {section:"httpd",
      key: "WWW-Authenticate",
      value:  "X-Couch-Test-Auth"}],
      
      function(){
        var designDoc = {
          _id:"_design/test",
          language: "javascript",
           _attachments:{
              "foo.txt": {
                content_type:"text/plain",
                data: "VGhpcyBpcyBhIGJhc2U2NCBlbmNvZGVkIHRleHQ="
              }
            },
          rewrite: "(" + (function (req) {
            path = "/" + req.path.join('/');

            if (path == "/forbidden") {
              throw {forbidden:
                  "path forbidden"};
            }
            if (path == "/unauthorized") {
                throw {unauthorized:
                    "path not authorized"};
            }
            if (path == "/notfound") {
                throw {not_found:
                    "path not found"};
            }

            if (path == "/foo") {
                return "foo.txt"
            }
            
            var matches = path.match(/^\/hello\/(\w.+)$/);
            
            if (matches[1]) {
              if (req.verb == "DELETE") {
                throw {forbidden:
                    "delete is forbidden"};
              }
              
              return "_update/hello/" + matches[1];
            }
            
            
            

          }).toString() + ")",
          updates: {
            "hello" : stringFun(function(doc, req) {
              if (!doc) {
                if (req.docId) {
                  return [{
                    _id : req.docId
                  }, "New World"]
                }
                return [null, "Empty World"];          
              }
              doc.world = "hello";
              doc.edited_by = req.userCtx;
              return [doc, "hello doc"];
            })
          }
        }

        db.save(designDoc);

        // test simple rewriting

        req = CouchDB.request("GET", "/test_suite_db/_rewrite/test/foo");
        T(req.responseText == "This is a base64 encoded text");
        T(req.getResponseHeader("Content-Type") == "text/plain");
        
        // test error 404
        
        req = CouchDB.request("GET", "/test_suite_db/_rewrite/test/notfound");
        T(req.status == 404);

        // test error 403
        try {
          req = CouchDB.request("GET", "/test_suite_db/_rewrite/test/forbidden");
          T(req.status == 403);
        } catch (e) {
          T(e.error == "forbidden");
          T(req.status == 403);
        }

        // test error 401
        try {
          req = CouchDB.request("GET", "/test_suite_db/_rewrite/test/unauthorized");
          T(req.status == 401);
        } catch (e) {
          T(req.status == 401);
          T(e.error == "unauthorized");
        }
        
        // test POST 
        // hello update world
        
        var doc = {"word":"plankton", "name":"Rusty"}
        var resp = db.save(doc);
        T(resp.ok);
        var docid = resp.id;
        
        xhr = CouchDB.request("PUT", "/test_suite_db/_rewrite/test/hello/"+docid);
        T(xhr.status == 201);
        T(xhr.responseText == "hello doc");
        T(/charset=utf-8/.test(xhr.getResponseHeader("Content-Type")))

        doc = db.open(docid);
        T(doc.world == "hello");
        
        
        try {
          req = CouchDB.request("DELETE", "/test_suite_db/_rewrite/test/hello/"+docid);
          T(req.status == 403);
        } catch (e) {
          T(e.error == "forbidden");
          T(req.status == 403);
        }
  });
  
}
  
  
  
  
  
  
  
  