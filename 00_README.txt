

    ELDE (Exhaustive Lock Dependency Emulator)
    (Created: 2018-04-13, Time-stamp: <2018-04-13T12:03:13Z>)


** 概要

ELDE (Exhaustive Lock Dependency Emulator) とは、プログラムのプロセスの
並列実行におけるロックの依存関係を総当り的に逐次実行で「エミュレート」
するフレームワークである。

現在は、ロックがグローバルな単一のロックしか扱えない。


** ファイルの内容

詳細は次のブログ記事を読んで欲しい。

  * elde_0.pl。「虚実行」を含む総当り。

    《Exhaustive Lock Dependency Emulator その１ 並列処理の総当り》
    http://jrf.cocolog-nifty.com/software/2011/06/post-1.html

  * elde_0wl.pl

    《Exhaustive Lock Dependency Emulator その２ wait_and_lock》
    http://jrf.cocolog-nifty.com/software/2011/07/post.html

  * elde_2.pl, elde_0_and_2.pl

    《Exhaustive Lock Dependency Emulator その３ 修正とチェック》
    http://jrf.cocolog-nifty.com/software/2018/04/post-1.html


** ライセンス

The author is a Japanese.

I intended this program to be public-domain, but you can treat
this program under the (new) BSD-License or under the Artistic
License, if it is convenient for you.

Within three months after the release of this program, I
especially admit responsibility of efforts for rational requests
of correction to this program.

I often have bouts of schizophrenia, but I believe that my
intention is legitimately fulfilled.


** 著者

JRF
( http://jrf.cocolog-nifty.com/software/ )


(This file was written in Japanese/UTF8.)
