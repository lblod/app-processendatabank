  # Run `docker-compose restart dispatcher` after updating
  # this file.

defmodule Dispatcher do
  use Matcher
  define_accept_types [
    html: ["text/html", "application/xhtml+html"],
    json: ["application/json", "application/vnd.api+json"],
    upload: ["multipart/form-data"],
    sparql_json: ["application/sparql-results+json"],
    any: [ "*/*" ],
  ]

  define_layers [ :api_services, :api, :frontend, :not_found ]
  
  get "/files/:id/download", %{ layer: :api_services } do
    Proxy.forward conn, [], "http://file/files/" <> id <> "/download"
  end

  post "/files/*path", %{ layer: :api_services } do
    Proxy.forward conn, path, "http://file/files/"
  end

  delete "/files/*path", %{ accept: [ :json ], layer: :api_services } do
    Proxy.forward conn, path, "http://file/files/"
  end
    
  get "/files/*path", %{ accept: [ :json ], layer: :api_services } do
    Proxy.forward conn, path, "http://resource/files/"
  end

  match "/bpmn-elements/*path",  %{ accept: [:json], layer: :api_services} do
    Proxy.forward conn, path, "http://resource/bpmn-elements/"
  end


  match "/bpmn/*path",  %{ accept: [:any], layer: :api} do
    Proxy.forward conn, path, "http://bpmn/"
  end

  ###############################################################
  # frontend layer
  ###############################################################

  match "/assets/*path", %{ layer: :api } do
    Proxy.forward conn, path, "http://frontend/assets/"
  end

  match "/@appuniversum/*path", %{ layer: :api } do
    Proxy.forward conn, path, "http://frontend/@appuniversum/"
  end

  match "/*path", %{ accept: [:html], layer: :api } do
    Proxy.forward conn, [], "http://frontend/index.html"
  end

  match "/*_path", %{ layer: :frontend } do
    Proxy.forward conn, [], "http://frontend/index.html"
  end

  match "/*_", %{ layer: :not_found } do
    send_resp( conn, 404, "Route not found.  See config/dispatcher.ex" )
  end
end
