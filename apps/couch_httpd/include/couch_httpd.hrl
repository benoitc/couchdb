% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-define(MIN_STR, <<"">>).
-define(MAX_STR, <<255>>). % illegal utf string

-record(httpd,
    {mochi_req,
    peer,
    method,
    requested_path_parts,
    path_parts,
    db_url_handlers,
    user_ctx,
    req_body = undefined,
    design_url_handlers,
    auth,
    default_fun,
    url_handlers
    }).

-record(extern_resp_args, {
    code = 200,
    stop = false,
    data = <<>>,
    ctype = "application/json",
    headers = []
}).

-record(http_db, {
    url,
    auth = [],
    resource = "",
    headers = [
        {"User-Agent", "CouchDB/"++couch:version()},
        {"Accept", "application/json"},
        {"Accept-Encoding", "gzip"}
    ],
    qs = [],
    method = get,
    body = nil,
    options = [
        {response_format,binary},
        {inactivity_timeout, 30000}
    ],
    retries = 10,
    pause = 500,
    conn = nil
}).

-record(view_query_args, {
    start_key,
    end_key,
    start_docid = ?MIN_STR,
    end_docid = ?MAX_STR,

    direction = fwd,
    inclusive_end=true, % aka a closed-interval

    limit = 10000000000, % Huge number to simplify logic
    skip = 0,

    group_level = 0,

    view_type = nil,
    include_docs = false,
    stale = false,
    multi_get = false,
    callback = nil,
    list = nil
}).


