# Kafka via kafkacat

defconfenv 'kafkacat', NI_KAFKACAT => 'kafkacat';


# kafka://broker:port: produce or consume messages across all topics
defresource 'kafka',
  read  => q{soproc {exec conf('kafkacat'), '-b', $_[1], '-C'}},
  write => q{siproc {exec conf('kafkacat'), '-b', $_[1], '-P'}};


# kafkat://broker:9092/topic: produce or consume messages within a specific
# topic
defresource 'kafkat',
  read  => q{my ($broker, $topic) = $_[1] =~ /([^\/]+)\/(.*)/;
             soproc {exec conf('kafkacat'), '-b', $broker, '-C', '-t', $topic}},
  write => q{my ($broker, $topic) = $_[1] =~ /([^\/]+)\/(.*)/;
             siproc {exec conf('kafkacat'), '-b', $broker, '-P', '-t', $topic}};
