# Change log

## 0.3.0.0

* Removed `TxErrorType` and change the `errorType` field on `TxException` to `errcode`.

## 0.2.0.0

* Changed the `TxM` encoding from a plain newtype-over-IO to a reader.
* Removed the `Tx` and `UnsafeTx` type classes.
* `TxEnv` is now a type class. Added `TxEnvs` for convenience.
* Added `TxException`.
* Added the `HEnv` type.
* Changed module structure so that unsafe functions are provided in an `Unsafe` module.

## 0.1.0.0

Initial release
