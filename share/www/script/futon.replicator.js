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

(function($) {
  $.futon = $.futon || {};
  $.extend($.futon, {
    
        
    CouchReplicatorPage: function() {
      var page = this;
           
      this.updateRecordsListing = function(db, options) {
        
        var db = db;
        if (options === undefined) options = {};
        if (options.limit === undefined) {
          var perPage = parseInt($("#perpage").val(), 10)
          // Fetch an extra row so we know when we're on the last page for
          // reduce views
          options.limit = perPage + 1;
        } else {
          perPage = options.limit - 1;
        }
        
        options.include_docs = true;
        
        
        if ($("#records thead th.key").is(".desc")) {
          if (typeof options.descending == 'undefined') options.descending = true;
          var descend = true;
          $.futon.storage.set("desc", "1");
        } else {
          var descend = false;
          $.futon.storage.del("desc");
        }
        $("#paging a").unbind();
        $("#records").find("tbody.content").empty().end().show();

        options.success = function(resp) {
          if (resp.offset === undefined) {
            resp.offset = 0;
          }
          var descending_reverse = ((options.descending && !descend) || (descend && (options.descending === false)));
          var has_reduce_prev = resp.total_rows === undefined && (descending_reverse ? resp.rows.length > perPage : options.startkey !== undefined);
          if (descending_reverse && resp.rows) {
            resp.rows = resp.rows.reverse();
            if (resp.rows.length > perPage) {
              resp.rows.push(resp.rows.shift());
            }
          }
          if (resp.rows !== null && (has_reduce_prev || (descending_reverse ?
            (resp.total_rows - resp.offset > perPage) :
            (resp.offset > 0)))) {
            $("#paging a.prev").attr("href", "#" + (resp.offset - perPage)).click(function() {
              var opt = {
                descending: !descend,
                limit: options.limit
              };
              if (resp.rows.length > 0) {
                var firstDoc = resp.rows[0];
                opt.startkey = firstDoc.key !== undefined ? firstDoc.key : null;
                if (firstDoc.id !== undefined) {
                  opt.startkey_docid = firstDoc.id;
                }
                opt.skip = 1;
              }
              page.updateRecordsListing(db, opt);
              return false;
            });
          } else {
            $("#paging a.prev").removeAttr("href");
          }
          var has_reduce_next = resp.total_rows === undefined && (descending_reverse ? options.startkey !== undefined : resp.rows.length > perPage);
          if (resp.rows !== null && (has_reduce_next || (descending_reverse ?
            (resp.offset - resp.total_rows < perPage) :
            (resp.total_rows - resp.offset > perPage)))) {
            $("#paging a.next").attr("href", "#" + (resp.offset + perPage)).click(function() {
              var opt = {
                descending: descend,
                limit: options.limit
              };
              if (resp.rows.length > 0) {
                var lastDoc = resp.rows[Math.min(perPage, resp.rows.length) - 1];
                opt.startkey = lastDoc.key !== undefined ? lastDoc.key : null;
                if (lastDoc.id !== undefined) {
                  opt.startkey_docid = lastDoc.id;
                }
                opt.skip = 1;
              }
              page.updateRecordsListing(db, opt);
              return false;
            });
          } else {
            $("#paging a.next").removeAttr("href");
          }

          for (var i = 0; i < Math.min(perPage, resp.rows.length); i++) {
            var row = resp.rows[i];
            var tr = $("<tr id='"+ $.couch.encodeDocId(row.id) + "'></tr>");
            
            var key = "null";
            if (row.key !== null) {
              key = $.futon.formatJSON(row.key, {indent: 0, linesep: ""});
            }
            var doc = row.value;
            
            if (doc.replication_id) {
              $("<td class='key'><a href='document.html?" + encodeURIComponent(db.name) +
                "/" + $.couch.encodeDocId(row.id) + "'><strong>" + $.futon.escape(row.id) + "</strong>")
                .appendTo(tr);
                
              $("<td class='repid'><div></div></td>").find("div")
              .html(doc.replication_id).end()
              .appendTo(tr);
              
              if (doc.state) {
                $("<td class='state'><div></div></td>").find("div")
                .html(doc.state).end()
                .appendTo(tr);
              } else {
                $("<td class='state'></td>").appendTo(tr);
              }
              
                
                
              $("<td class='source'><div></div></td>").find("div")
              .html(doc.source).end()
              .appendTo(tr);
              
               $("<td class='target'><div></div></td>").find("div")
                .html(doc.target).end()
                .appendTo(tr);
              
              $('<td><div style="text-align:center;"">' +
              '<a class="remove" href="#remove">x</a></div></td>')
              .appendTo(tr);
              
            }

            tr.find(".remove").click(function(e) {
              e.preventDefault();
              var line = $(this).parent().parent().parent();
              var docid = line.attr("id");
              db.openDoc(docid, {
                success: function(doc) {
                  db.removeDoc(doc, {
                    success: function() {
                      line.remove();
                    }
                  });
                }
              });
              return false;
            })

            tr.appendTo("#records tbody.content");

            
          }
          
          
          var firstNum = 1;
          var lastNum = totalNum = Math.min(perPage, resp.rows.length);
          if (resp.total_rows != null) {
            if (descending_reverse) {
              lastNum = Math.min(resp.total_rows, resp.total_rows - resp.offset);
              firstNum = lastNum - totalNum + 1;
            } else {
              firstNum = Math.min(resp.total_rows, resp.offset + 1);
              lastNum = firstNum + totalNum - 1;
            }
            totalNum = resp.total_rows;
          } else {
            totalNum = "unknown";
          }
          $("#paging").show();

          $("#records tbody.footer td span").text(
            "Showing " + firstNum + "-" + lastNum + " of " + totalNum +
            " row" + (firstNum != lastNum || totalNum == "unknown" ? "s" : ""));
          $("#records tbody tr:odd").addClass("odd");
        }
        options.error = function(status, error, reason) {
          alert("Error: " + error + "\n\n" + reason);
        }

        db.view("_replicator/futon", options);
      }
      
     
      $.couch.config({
        success: function(dbname) {
          var db = $.couch.db(dbname);
          page.updateRecordsListing(db);
          $("#perpage").change(function() {
              page.updateRecordsListing(db);
              $.futon.storage.set("per_page", this.value);
          });
            
          db.info({
            success: function(info) {
              db.changes(info.update_seq).onChange(function() {
                page.updateRecordsListing(db);
              });
            }
          });
          
          
        }
        
      },"replicator", "db");
    }
    
  });
})(jQuery);