Программа написана на языке erlang. Для исполнения требует наличие установленой erlang-машины с `escript` и `run_erl`, а также утилиты `make`.

### Структура приложения

Прорамма представляет из себя erlang приложение `king`

В нём есть супервайзер, управляющий одним `gen_fsm` - `king_node`. Там содержится логика выборов короля.

Таймаут пинга вождя задаётся в application environment (`src/king.app.src`)

Таёмаут ожидания ответа вождя в 4 раза больше

Информация о номерах и именах нод также задаётся в application environment с ключом `nodes`

За разруливание этих имён отвечает модуль `king_nodes`

Фасад для запроса вождя и запуска приложения - модуль `king` с функциями `start/0`, `who_is_the_king/0`

### Сборка и запуск

Для сборки и запуска используется `rebar` и `make`

Компиляция - `make compile`

Запуск одной ноды - `NAME=node_name make start`

Запуск набора тестовых нод - `make start_test_nodes`

Для запроса кололя из консоли используется `escript`

Запрос короля - `make who` - выводит имя ноды текущего короля


