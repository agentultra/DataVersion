# DataMigration

Type safe data migrations.

All you need to do is create a type family to index your record and
provide an instance of `Transform` to migrate your data between
versions.

Migrations are type-safe and the library uses generics to remove as
much boiler-plate as possible.

## Future Considerations

In the future this library will provide a high-level DSL to enable
better ergonomics around type errors so that you can see which fields
require specification in the migration.
