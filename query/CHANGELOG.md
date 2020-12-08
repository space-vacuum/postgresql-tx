# Change log

## 0.3.0.0

* Updated for postgresql-tx-0.3.0.0.

## 0.2.0.0

* Updated for postgresql-tx-0.2.0.0.
* Added `HasCallStack` constraints in transaction runners.
* Added more transaction runners: `pgWithTransactionSerializable`, `pgWithTransactionModeRetry`.
* Added `pgWithSavepoint`.
* Added `pgQueryWithMasker`, `pgExecuteWithMasker`.
* Moved internals to an `Internal` module.

## 0.1.0.0

* Initial release
