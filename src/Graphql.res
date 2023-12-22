open Helpers

type graphQLString
type graphQlObject

// for integration with graphql-relay
type interface
type connectionArgs<'a>

module Input = {
  type t<'t> = {
    name: string,
    description: Js.undefined<string>,
    interfaces: Js.undefined<unit => array<interface>>,
    fields: 't,
  }
}

module Types = {
  type t

  @module("graphql") external stringType: t = "GraphQLString"
  @module("graphql") external intType: t = "GraphQLInt"
  @module("graphql") external floatType: t = "GraphQLFloat"
  @module("graphql") external idType: t = "GraphQLID"
  @module("graphql") external booleanType: t = "GraphQLBoolean"

  @module("graphql") @new external required: t => t = "GraphQLNonNull"
}

module Field = {
  type t<'source, 'args, 'ctx, 'a> = ('source, 'args, 'ctx) => 'a

  type resolverOutput

  module Resolver = {
    type t<'source, 'args, 'ctx> = ('source, 'args, 'ctx) => promise<Js.Null.t<resolverOutput>>

    let identity: 'a => 'b = %raw("function(a) {return a}")

    let make = (r: option<('source, 'args, 'ctx) => 'a>): option<
      ('source, 'args, 'ctx) => promise<Js.Null.t<resolverOutput>>,
    > => {
      Js.Option.map((. i) => identity(i), r)
    }
  }

  type field2 = {@as("type") type_: Types.t, description: string}

  type field3<'source, 'args, 'ctx> = {
    type_: Types.t,
    description: string,
    resolve: option<Resolver.t<'source, 'args, 'ctx>>,
  }

  type fieldFull<'source, 'args, 'ctx> = {
    type_: Types.t,
    description: option<string>,
    args: option<connectionArgs<'args>>,
    resolve: option<Resolver.t<'source, 'args, 'ctx>>,
  }

  type field<'source, 'args, 'ctx> = @unwrap
  [
    | #Field2(field2)
    | #Field3(field3<'source, 'args, 'ctx>)
    | #FieldFull(fieldFull<'source, 'args, 'ctx>)
  ]

  type fieldCallback<'source, 'args, 'ctx> = ('source, 'args, 'ctx) => field<'source, 'args, 'ctx>

  type newField<'source, 'args, 'ctx> = [
    | #Field2(field2)
    | #Field3(field3<'source, 'args, 'ctx>)
    | #FieldFull(fieldFull<'source, 'args, 'ctx>)
    | #Callback(fieldCallback<'source, 'args, 'ctx>)
  ]

  type fields<'source, 'args, 'ctx> = Js.Dict.t<newField<'source, 'args, 'ctx>>

  let empty = (): fields<'source, 'args, 'ctx> => Js.Dict.empty()

  let addField = (fields, key, field: newField<'source, 'args, 'ctx>): fields<
    'source,
    'args,
    'ctx,
  > => {
    fields->Js.Dict.set(key, field)
    fields
  }

  module Internal = {
    type field3__internal<'source, 'args, 'ctx> = {
      @as("type") type_: Types.t,
      description: string,
      resolve: Js.undefined<Resolver.t<'source, 'args, 'ctx>>,
    }

    type fieldFull__internal<'source, 'args, 'ctx> = {
      @as("type") type_: Types.t,
      description: Js.undefined<string>,
      args: Js.undefined<connectionArgs<'args>>,
      resolve: Js.undefined<Resolver.t<'source, 'args, 'ctx>>,
    }

    let makeField = (key, field: field<'source, 'args, 'ctx>) => {
      switch field {
      | #Field2(f) => jsCreateObj(key, f)
      | #Field3(f) => {
          let i: field3__internal<'source, 'args, 'ctx> = {
            type_: f.type_,
            description: f.description,
            resolve: f.resolve->Js.Undefined.fromOption,
          }
          jsCreateObj(key, i)
        }
      | #FieldFull(f) => {
          let i: fieldFull__internal<'source, 'args, 'ctx> = {
            type_: f.type_,
            description: f.description->Js.Undefined.fromOption,
            args: f.args->Js.Undefined.fromOption,
            resolve: f.resolve->Js.Undefined.fromOption,
          }
          jsCreateObj(key, i)
        }
      }
    }
  }

  let make = (fields: fields<'source, 'args, 'ctx>): t<'source, 'args, 'ctx, 'a> => {
    let i = (obj, args, ctx) => {
      let jsObj = Js.Obj.empty()
      fields
      ->Js.Dict.entries
      ->Js.Array2.forEach(((key, field)) => {
        switch field {
        | #Field2(f) => jsObj->Js.Obj.assign(jsCreateObj(key, f))->ignore
        | #Field3(f) => {
            let i: Internal.field3__internal<'source, 'args, 'ctx> = {
              type_: f.type_,
              description: f.description,
              resolve: f.resolve->Js.Undefined.fromOption,
            }
            jsObj->Js.Obj.assign(jsCreateObj(key, i))->ignore
          }
        | #FieldFull(f) => {
            let i: Internal.fieldFull__internal<'source, 'args, 'ctx> = {
              type_: f.type_,
              description: f.description->Js.Undefined.fromOption,
              args: f.args->Js.Undefined.fromOption,
              resolve: f.resolve->Js.Undefined.fromOption,
            }
            jsObj->Js.Obj.assign(jsCreateObj(key, i))->ignore
          }
        | #Callback(f) => {
            let i = Internal.makeField(key, f(obj, args, ctx))
            jsObj->Js.Obj.assign(i)->ignore
          }
        }
      })
      ->ignore
      jsObj
    }
    i->Resolver.identity
  }
}

module ModelType = {
  type m<'source, 'args, 'ctx, 'a> = {
    name: string,
    description: option<string>,
    interfaces: option<unit => array<interface>>,
    fields: Field.t<'source, 'args, 'ctx, 'a>,
  }

  module Internal = {
    type t<'source, 'args, 'ctx, 'a> = {
      name: string,
      description: Js.undefined<string>,
      interfaces: Js.undefined<unit => array<interface>>,
      fields: Field.t<'source, 'args, 'ctx, 'a>,
    }

    @module("graphql") @new
    external newGraphqlObjectType: t<'source, 'args, 'ctx, 'a> => Types.t = "GraphQLObjectType"
  }

  let make = (model: m<'source, 'args, 'ctx, 'a>) =>
    Internal.newGraphqlObjectType({
      name: model.name,
      description: model.description->Js.Undefined.fromOption,
      interfaces: model.interfaces->Js.Undefined.fromOption,
      fields: model.fields,
    })
}

module Model = {
  type m

  type resolver<'source, 'args, 'ctx, 'a> = ('source, 'args, 'ctx) => promise<Js.Null.t<'a>>

  type t<'source, 'args, 'ctx, 'a> = {
    @as("type") type_: Types.t,
    args: option<connectionArgs<'args>>,
    resolve: resolver<'source, 'args, 'ctx, 'a>,
  }

  let make: t<'source, 'args, 'ctx, 'a> => m = %raw("function(v) {return v}")
}

module Query = {
  type q

  type t = {
    name: string,
    fields: Js.Dict.t<Model.m>,
  }

  let empty = name => {name, fields: Js.Dict.empty()}

  let addField = (query, key, field) => {
    query.fields->Js.Dict.set(key, field)
    query
  }

  module Internal = {
    type t = {
      name: string,
      fields: unit => Js.Dict.t<Model.m>,
    }
    @module("graphql") @new
    external make: t => q = "GraphQLObjectType"
  }

  let make = (query: t): q =>
    Internal.make({
      name: query.name,
      fields: () => query.fields,
    })
}

module Mutation = {
  type m

  type relayMutation<'inputDef, 'outputDef, 'output, 'ctx, 'd> = {
    name: string,
    description: Js.undefined<string>,
    inputFields: Js.Dict.t<'inputDef>,
    outputFields: 'outputDef,
    mutateAndGetPayload: ('output, 'ctx) => 'd,
  }
}

module Schema = {
  type t

  type schemaConfig = {
    query: option<Query.q>,
    mutation: option<Mutation.m>,
  }

  module Internal = {
    type schemaConfig__internal<'t, 'f> = {
      query: Js.undefined<'t>,
      mutation: Js.undefined<'f>,
    }

    @module("graphql") @new
    external make__internal: schemaConfig__internal<'q, 'm> => t = "GraphQLSchema"
  }

  let make = (config: schemaConfig) =>
    Internal.make__internal({
      query: config.query->Js.Undefined.fromOption,
      mutation: config.mutation->Js.Undefined.fromOption,
    })

  @module("graphql") external printSchema: t => string = "printSchema"

  module Fs = {
    @module("fs") external writeFile: (string, string) => unit = "writeFileSync"
  }
}

module Self = {
  type t = {
    schema: Schema.t,
    source: string,
  }

  @module("graphql")
  external make: t => promise<'a> = "graphql"
}
