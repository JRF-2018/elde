# Exhaustive Lock Dependency Emulator

<!-- Time-stamp: "2020-04-08T06:12:53Z" -->

マルチスレッドまたはマルチプロセスの最も簡易な lock/unlock 機構におい
て、その依存を総当たり的に調べるプログラムを(Perlで)書いた。

これはマルチプロセス環境を Perl 上でエミュレートするという話ではまった
くない。プログラム自体は、シングルプロセスで終了する形をとる。

「虚実行」「実実行」の考え方の提案などがある。

詳しいことは↓から辿れる。

《Exhaustive Lock Dependency Emulator その１ 並列処理の総当り - JRF のソフトウェア Tips》  
http://jrf.cocolog-nifty.com/software/2011/06/post-1.html

更新情報は↓で。

《Exhaustive Lock Dependency Emulator その３ 修正とチェック - JRF のソフトウェア Tips》  
http://jrf.cocolog-nifty.com/software/2018/04/post-1.html


## GitHub 登録までの略歴

2011-06-20、初公開。2018-04-13、バージョン 0.03。2020-04-08、GitHub に
バージョン 0.03 を初登録。


## ライセンス

パブリックドメイン。 (数式のような小さなプログラムなので。)

自由に改変・公開してください。


----
(This document is mainly written in Japanese/UTF8.)
