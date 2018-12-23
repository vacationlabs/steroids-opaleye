{-|

= Motivation - Why should you use this library?

One needs to write a considerable amount of boilerplate to use 'Opaleye'. This
library alleviates the pain of hand-writing boilerplate code, by auto-generating
it based on your Postgres schema.

TODO - INSERT EXAMPLE

= Alternatives - What else can you use?

TODO - talk about opaleye-gen

= Headline features

- Generate completely polymorphic records based on a given Postgres schema - one
  record for each table

- Generate @ProductProfunctor@ instance and 'Opaleye.Table' declaration for each table/record.

- Generate concrete types for these polymorphic records based on default DB <>
  Haskell data-type mappings (you can change these mappings if you don't like
  the defaults). For example, a @UserPoly@ base record would also get,
  @UserPGW@, @UserPGR@, @UserW@, and @User@ concrete types.

- Generate lenses for these polymorphic records - based on /physical code-gen/,
  not Template Haskell.

- Generate a newtype for the primary-key of each table, eg. @UserId@, @OrderId@, @InviceId@, etc.

- Generate a sum-type for each @ENUM@ field along with the related boilerplate,
  i.e. conversion to/from string values, JSON instances, @ToField@ & @FromField@
  instances, @QueryRunnerColumnDefault@ & @Default Constant@ instances. For
  example, a @status@ column that can only have hold @"incomplete"@,
  @"complete"@, @"processing"@, @"shipped"@, can be mapped to the following on
  the Haskell side: @data Status = Incomplete | Complete | Processing | Shipped@

= Using the command-line tool


== Basic usage

@
steroids-opaleye
  --pg-user
  --pg-host
  --pg-port
  --pg-db
  --pg-password
  --include-schema-regex
  --include-table-regex
  --exclude-table-regex
  --output-dir
@

== Changing DB<>Haskell type-mappings

@
steroids-opaleye
  --pg-user
  --pg-host
  --pg-port
  --pg-db
  --pg-password
  --output-dir
  --type-mapping=jsonb,Data.Aeson.Value
@

== Generating lenses

@
steroids-opaleye
  --pg-user
  --pg-host
  --pg-port
  --pg-db
  --pg-password
  --output-dir
  --type-mapping=jsonb,Data.AesonValue
  --generate-lenses=[none,common-classes,individual-classes]
@

== Generating newtypes for primary keys and using them for foreign keys

@
steroids-opaleye
  --pg-user
  --pg-host
  --pg-port
  --pg-db
  --pg-password
  --output-dir
  --pk-newtypes
  --fk-newtypes
@

== Generating sum-types for @ENUM@s


=== Based only on @ENUM@ types in the PG schema

@
steroids-opaleye
  --pg-user
  --pg-host
  --pg-port
  --pg-db
  --pg-password
  --output-dir
  --pk-newtypes
  --enum-types
@

=== Manually specified @ENUM@s for any text field

@
steroids-opaleye
  --pg-user
  --pg-host
  --pg-port
  --pg-db
  --pg-password
  --output-dir
  --manual-enum=[[schema.]table_name.col_name],val1,val2,val3,val4
@


= Using the Haskell library programmatically

== Basic usage

@
autoGen :: IO ()
autoGen = do
@

TODO

= 

-}
module Steroids where

import Prelude()
