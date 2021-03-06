erl -config ./app.config \
    -sname devsim \
    -mnesia dir '"_db/"' \
    -pa _build/default/lib/devsim/ebin \
    -pa _build/default/lib/lager/ebin/ \
    -pa _build/default/lib/goldrush/ebin \
    -pa _build/default/lib/cowlib/ebin \
    -pa _build/default/lib/cowboy/ebin \
    -pa _build/default/lib/ranch/ebin \
    -pa _build/default/lib/hackney/ebin \
    -pa _build/default/lib/idna/ebin \
    -pa _build/default/lib/unicode_util_compat/ebin \
    -pa _build/default/lib/mimerl/ebin \
    -pa _build/default/lib/certifi/ebin \
    -pa _build/default/lib/ssl_verify_fun/ebin \
    -pa _build/default/lib/metrics/ebin \
    -setcookie MyCookie \
    -eval "application:start(devsim)"

