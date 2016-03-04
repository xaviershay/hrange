require 'yaml'

hosts = (0..1000).map do |x|
  "host#{x}"
end

(0..1000).each do |x|
  name = "clusters#{x}"
  data = {
    'CLUSTER' => hosts.sample(rand(100)),
    'ONE'     => hosts.sample(1)
  }
  fn = "bench/#{name}.yaml"

  File.write(fn, data.to_yaml)
end
