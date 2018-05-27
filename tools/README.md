
# Steps to prepare system api stubs with api-helper.py

## A) Apex

1. Retrieve & restructure completions from the tooling api (-retrieve)
2. Check for interfaces (-interfaces)
3. Extract class data & method signatures from the documentation (-retrievedocs)
4. Enable in your dev org all user licenses (in User+Profile)
3. Use TypeWrapper to get inheritances: Save & execute generated class in salesforce. (-getwrapper)
4. Download Parents.txt and add inheritance infos. (-inheritance)
5. Get properties information: static & type. (-properties)
6. Prepare a package binary (java -jar target/apextools-0.1.0-SNAPSHOT-standalone.jar prepare-api --path /path/to/api.json)
7. Put api.bin in resources directory.

## B) SObjects

1. Retrieve & restructure sobject info from the tooling api (-sobjects).
2. Prepare a package binary (java -jar target/apextools-0.1.0-SNAPSHOT-standalone.jar prepare-schema --path /path/to/schema.json)
3. Put schema.bin in resources directory.

Names are api40.bin & schema40.bin depending on the code references.