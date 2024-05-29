  # Run `docker-compose restart dispatcher` after updating
  # this file.

defmodule Dispatcher do
  use Matcher
  define_accept_types [
    html: ["text/html", "application/xhtml+html"],
    json: ["application/json", "application/vnd.api+json"],
    upload: ["multipart/form-data"],
    sparql_json: ["application/sparql-results+json"],
    bpmn_converted: ["image/png", "image/svg+xml", "application/pdf"],
    bpmn_original: ["text/xml", "application/xml"],
    any: [ "*/*" ],
  ]

  define_layers [ :api_services, :api, :frontend, :not_found ]

  match "/bpmn-elements",  %{ accept: [:json], layer: :api } do
    Proxy.forward conn, [], "http://cache/bpmn-elements/"
  end

  match "/bpmn-element-types", %{ accept: [:json], layer: :api } do
    Proxy.forward conn, [], "http://cache/bpmn-element-types/"
  end

  post "/bpmn",  %{ accept: [:any], layer: :api } do
    Proxy.forward conn, [], "http://bpmn/"
  end

  ###############################################################
  # files
  ###############################################################

  get "/files/:id/download", %{ accept: %{ bpmn_converted: true }, layer: :api_services } do
    IO.inspect( conn, label: "BPMN converted" )
    Proxy.forward conn, [], "http://bpmn/" <> id <> "/download"
  end

  get "/files/:id/download", %{ accept: %{ bpmn_original: true }, layer: :api_services } do
    IO.inspect( conn, label: "BPMN original" )
    Proxy.forward conn, [], "http://file/files/" <> id <> "/download"
  end

  post "/files/*path", %{ layer: :api_services } do
    Proxy.forward conn, path, "http://file/files/"
  end

  delete "/files/*path", %{ accept: [ :json ], layer: :api_services } do
    Proxy.forward conn, path, "http://file/files/"
  end

  get "/files/*path", %{ accept: [ :json ], layer: :api_services } do
    Proxy.forward conn, path, "http://cache/files/"
  end

  patch "/files/*path", %{ accept: [ :json ], layer: :api_services } do
    Proxy.forward conn, path, "http://cache/files/"
  end

  ###############################################################
  # login
  ###############################################################

  match "/sessions/*path" do
    Proxy.forward conn, path, "http://login/sessions/"
  end

  match "/accounts", %{ accept: [:json], layer: :api} do
    Proxy.forward conn, [], "http://cache/accounts/"
  end

  match "/accounts/*path", %{ accept: [:json], layer: :api} do
    Proxy.forward conn, path, "http://accountdetail/accounts/"
  end

  match "/groups/*path", %{ accept: [:json], layer: :api} do
    Proxy.forward conn, path, "http://cache/groups/"
  end

  match "/sites/*path", %{ accept: [:json], layer: :api} do
    Proxy.forward conn, path, "http://cache/sites/"
  end

  match "/mock/sessions/*path", %{ accept: [:any], layer: :api} do
    Proxy.forward conn, path, "http://mock-login/sessions/"
  end

  ###############################################################
  # SEARCH
  ###############################################################

  match "/search/*path", %{  accept: %{ json: true }, layer: :api} do
    Proxy.forward conn, path, "http://search/"
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
