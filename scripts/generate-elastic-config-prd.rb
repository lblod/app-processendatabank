require 'json'
require 'fileutils'
require 'bundler/inline'

gemfile do
  source 'https://rubygems.org'
  gem 'linkeddata'
end

SPARQL_CLIENT = "http://localhost:8890/sparql"
CONFIG_FILE = 'config/search/dev/config.json'
OUTPUT_FILE = 'config/search/prd/config.json'

def client
    @client ||= SPARQL::Client.new(SPARQL_CLIENT)
end

def query
    q = %|
            PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
            PREFIX mu: <http://mu.semte.ch/vocabularies/core/>

            SELECT DISTINCT ?s ?uuid ?label WHERE {
              ?s a besluit:Bestuurseenheid;
                skos:prefLabel ?label;
                mu:uuid ?uuid.
            }
            ORDER BY ?label ?uuid
        |
    return client.query(q)
end

def get_eager_index_groups
    data = query;
    search_conf = [];
    data.each do |sg|
      json = [
        { "name": "authenticated", "variables": [] },
        { "name": "org", "variables": [sg.uuid.value] }
      ]
      search_conf << json
    end
    search_conf
end

def generate_config
  config = JSON.parse(File.read(CONFIG_FILE))

  config['eager_indexing_groups'] += get_eager_index_groups

  FileUtils.mkdir_p(File.dirname(OUTPUT_FILE))
  File.open(OUTPUT_FILE, 'w') do |f|
    f.write(JSON.pretty_generate(config))
  end
end

generate_config