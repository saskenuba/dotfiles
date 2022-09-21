{:repl      {:dependencies [[nrepl/nrepl "1.0.0"]]
             :plugins      [[com.billpiel/sayid "0.1.0"]
                            [mx.cider/enrich-classpath "1.9.0"]]}
 :dev-local {:dependencies [[com.datomic/dev-local "0.9.235"]
                            [com.github.jpmonettas/flow-storm-dbg "2.3.141"]
                            [com.github.jpmonettas/flow-storm-inst "2.3.141"]]
             :plugins      [[lein-kibit "0.1.8"]]
             :env          {:env-tag                  "dev"
                            :aws-endpoint             "http://localhost:4566"
                            :aws-profile              "zougue-dev"
                            :vtex-api-endpoint-prefix "http://localhost:8890"
                            :http-port                "8890"

                            :db-server-type "dev-local"
                            :db-system      "zougue-mpms-dev"
                            :db-name        "zougue-mpms"

                            :s3-bucket-images "zougue-dev-mpms-images"
                            :s3-bucket-docs   "zougue-dev-mpms-docs"

                            :jobs-enabled   "true"
                            :jobs-sqs-queue "http://localhost:4566/000000000000/zougue-dev-mpms-jobs"}}}
