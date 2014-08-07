benchmark
===

libraries
---
* apiary
* scotty
* Spock

how to run
---
```.sh
cabal update
cabal sandbox init
cabal install --only-dependencies
cabal configure
cabal build
./bench apiary HELLO > result.log
```

benchmarks
---
1. HELLO (no capture)
2. PARAM (capture route parameter)
3. DEEP  (deep and match route)
3. AFTER_DEEP (after DEEP route)


machines
---

### machine1

```.sh
% uname -a
Linux machine1 3.2.0-4-amd64 #1 SMP Debian 3.2.57-3+deb7u2 x86_64 GNU/Linux
% cat /proc/cpuinfo | grep 'model name'
model name	: Intel(R) Core(TM) i3-2120T CPU @ 2.60GHz
model name	: Intel(R) Core(TM) i3-2120T CPU @ 2.60GHz
model name	: Intel(R) Core(TM) i3-2120T CPU @ 2.60GHz
model name	: Intel(R) Core(TM) i3-2120T CPU @ 2.60GHz
% cat /proc/meminfo | grep MemTotal
MemTotal:       16354960 kB
```

results
---

|machine|  ghc  |  framework | version  |   mode   |  bench1  |  bench2  |  bench3  |  bench4  |
|-------|-------|------------|----------|----------|----------|----------|----------|----------|
|      1|7.8.2  |apiary      |0.14.0.1  |  30s * 10|  38735.36|  33697.38|   8673.75|  10027.99|
|      1|7.8.2  |apiary      |0.15.0    |  30s * 10|  36327.45|  32008.10|  33772.90|  38177.57|
|      1|7.8.2  |scotty      |0.8.2     |  30s * 10|  29986.62|  23129.70|   2610.06|   9290.42|
|      1|7.8.2  |Spock       |0.6.1.3   |  30s * 10|  31548.67|  29582.84|  27770.14|  32874.91|

references
---
1. [agrafix/Spock-scotty-benchmark](https://github.com/agrafix/Spock-scotty-benchmark)