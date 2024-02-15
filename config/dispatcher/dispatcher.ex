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

  match "/bpmn-elements",  %{ accept: [:json], layer: :api } do
    Proxy.forward conn, [], "http://resource/bpmn-elements/"
  end

  post "/bpmn",  %{ accept: [:any], layer: :api } do
    Proxy.forward conn, [], "http://bpmn/"
  end

  ###############################################################
  # files
  ###############################################################

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

  ###############################################################
  # login
  ###############################################################

  match "/mock/sessions/*path", %{ accept: [:any], layer: :api} do
    Proxy.forward conn, path, "http://mock-login/sessions/"
  end

  ###############################################################
  # frontend
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

  ###############################################################
  # errors
  ###############################################################

  match "/*_path", %{ accept: [:any], layer: :not_found} do
    send_resp( conn, 404, "{\"error\": {\"code\": 404}")
  end
end
