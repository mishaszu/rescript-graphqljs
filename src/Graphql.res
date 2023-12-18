open Helpers

type graphQLString
type graphQlObject

// for integration with graphql-relay
type interface
type connectionArgs

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
  type fields

  type resolver<'source, 'args, 'ctx, 'a> = ('source, 'args, 'ctx) => promise<Js.Null.t<'a>>

  type field2 = {"type": Types.t, "description": string}

  type field3<'source, 'args, 'ctx, 'a> = {
    type_: Types.t,
    description: string,
    resolve: option<resolver<'source, 'args, 'ctx, 'a>>,
  }

  type __field3Internal<'source, 'args, 'ctx, 'a> = {
    @as("type") type_: Types.t,
    description: string,
    resolve: Js.undefined<resolver<'source, 'args, 'ctx, 'a>>,
  }

  type fieldFull<'source, 'args, 'ctx, 'a> = {
    type_: Types.t,
    description: option<string>,
    args: option<connectionArgs>,
    resolve: option<resolver<'source, 'args, 'ctx, 'a>>,
  }

  type __fieldFullInternal<'source, 'args, 'ctx, 'a> = {
    @as("type") type_: Types.t,
    description: Js.undefined<string>,
    args: Js.undefined<connectionArgs>,
    resolve: Js.undefined<resolver<'source, 'args, 'ctx, 'a>>,
  }

  type t<'source, 'args, 'ctx, 'a> = @unwrap
  [
    | #Field2(field2)
    | #Field3(field3<'source, 'args, 'ctx, 'a>)
    | #FieldFull(fieldFull<'source, 'args, 'ctx, 'a>)
  ]

  type fieldCallback<'source, 'args, 'ctx, 'a> = (
    'source,
    'args,
    'ctx,
  ) => t<'source, 'args, 'ctx, 'a>

  let makeField = (key, field: t<_, _, _, _>) => {
    switch field {
    | #Field2(f) => jsCreateObj(key, f)
    | #Field3(f) => {
        let i = {
          type_: f.type_,
          description: f.description,
          resolve: f.resolve->Js.Undefined.fromOption,
        }
        jsCreateObj(key, i)
      }
    | #FieldFull(f) => {
        let i = {
          type_: f.type_,
          description: f.description->Js.Undefined.fromOption,
          args: f.args->Js.Undefined.fromOption,
          resolve: f.resolve->Js.Undefined.fromOption,
        }
        jsCreateObj(key, i)
      }
    }
  }

  let dictToObj = (fields: Js.Dict.t<t<_, _, _, _>>) => {
    let obj = Js.Obj.empty()
    fields
    ->Js.Dict.entries
    ->Js.Array2.forEach(((k, f)) => {
      obj->Js.Obj.assign(makeField(k, f))->ignore
    })
    obj
  }

  let __anythingToField: 'a => fields = %raw("function(fields) { return fields }")

  let make = (fields: Js.Dict.t<t<_, _, _, _>>) => {
    fields->dictToObj->__anythingToField
  }

  let makeCallback = (fields: Js.Dict.t<fieldCallback<_, _, _, _>>, graphqlObj, args, context) => {
    let obj = Js.Obj.empty()
    fields
    ->Js.Dict.entries
    ->Js.Array2.forEach(((key, field)) => {
      let field = field(graphqlObj, args, context)
      obj->Js.Obj.assign(makeField(key, field))->ignore
    })
    obj
  }
}

module Model = {
  type m = {
    "name": string,
    "description": option<string>,
    "interfaces": option<unit => array<interface>>,
    "fields": Field.fields,
  }

  type t = {
    name: string,
    description: Js.undefined<string>,
    interfaces: Js.undefined<unit => array<interface>>,
    fields: Field.fields,
  }

  @module("graphql") @new
  external newGraphqlObjectType: t => graphQlObject = "GraphQLObjectType"

  let make = model =>
    newGraphqlObjectType({
      name: model["name"],
      description: model["description"]->Js.Undefined.fromOption,
      interfaces: model["interfaces"]->Js.Undefined.fromOption,
      fields: model["fields"],
    })
}

module Query = {
  type graphqlQueryObject<'t> = {
    name: string,
    fields: unit => 't,
  }
}

module Mutation = {
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

  type schemaConfig<'t, 'f> = {
    query: Js.undefined<'t>,
    mutation: Js.undefined<'f>,
  }

  @module("graphql") @new
  external make: schemaConfig<'q, 'm> => t = "GraphQLSchema"

  @module("graphql") external printSchema: t => string = "printSchema"

  module Fs = {
    @module("fs") external writeFile: (string, string) => unit = "writeFileSync"
  }
}
