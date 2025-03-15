# Running the Lox tests

This requires an appropriate Dart version.

I have modified the test code locally to run
with a newer Dart version, but those changes are not
complete so I have not published a fork yet.

```bash
cd ../ci-book && \
    dart tool/bin/test.dart chap04_scanning \
         --interpreter ../rust-lox/target/debug/rust-lox.exe \
         --arguments "--std,tokenize"
```
