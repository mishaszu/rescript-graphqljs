open Helpers
type t

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

module Model = {
  type t<'t> = {
    name: string,
    description: Js.undefined<string>,
    interfaces: Js.undefined<unit => array<interface>>,
    fields: 't,
  }
}

module InputTypes = {
  type t<'a>

  @module("graphql") @new
  external newGraphqlObjectType: Model.t<'t> => graphQlObject = "GraphQLObjectType"

  @module("graphql") external stringType: t<string> = "GraphQLString"
  @module("graphql") external intType: t<int> = "GraphQLInt"
  @module("graphql") external floatType: t<float> = "GraphQLFloat"
  @module("graphql") external idType: t<string> = "GraphQLID"
  @module("graphql") external booleanType: t<bool> = "GraphQLBoolean"

  @module("graphql") @new external required: t<'a> => t<'a> = "GraphQLNonNull"
}

module Field = {
  type field2<'a> = {@as("type") type_: InputTypes.t<'a>, description: string}

  type resolver<'source, 'args, 'ctx, 'a> = ('source, 'args, 'ctx) => promise<Js.Null.t<'a>>

  type field3<'source, 'args, 'ctx, 'a, 'fieldType> = {
    @as("type") type_: InputTypes.t<'fieldType>,
    description: string,
    resolve: Js.undefined<resolver<'source, 'args, 'ctx, 'a>>,
  }

  type fieldFull<'source, 'args, 'ctx, 'a, 'fieldType> = {
    @as("type") type_: InputTypes.t<'fieldType>,
    description: Js.undefined<string>,
    args: Js.undefined<connectionArgs>,
    resolve: Js.undefined<resolver<'source, 'args, 'ctx, 'a>>,
  }

  type fieldWithArgs<'source, 'args, 'ctx, 'a, 'fieldType> = {
    @as("type") type_: InputTypes.t<'fieldType>,
    description: Js.undefined<string>,
    args: 'args,
    resolve: resolver<'source, 'args, 'ctx, 'a>,
  }

  type type_<'source, 'args, 'ctx, 'a, 'fieldType> =
    | Field2(field2<'fieldType>)
    | Field3(field3<'source, 'args, 'ctx, 'a, 'fieldType>)
    | FieldFull(fieldFull<'source, 'args, 'ctx, 'a, 'fieldType>)
    | FieldWithArgs(fieldWithArgs<'source, 'args, 'ctx, 'a, 'fieldType>)

  type fieldBuilder<'source, 'args, 'ctx, 'a, 'fieldType> = Js.Dict.t<
    type_<'source, 'args, 'ctx, 'a, 'fieldType>,
  >

  let createFields = () => Js.Obj.empty()

  type fieldsDict<'source, 'args, 'ctx, 'a, 'fieldType> = Js.Dict.t<
    type_<'source, 'args, 'ctx, 'a, 'fieldType>,
  >

  type fieldCallback<'source, 'args, 'ctx, 'a, 'fieldType> = (
    graphQlObject,
    'ctx,
  ) => type_<'source, 'args, 'ctx, 'a, 'fieldType>

  let addFieldInCallback = (
    devObj,
    key: string,
    cb: fieldCallback<'source, 'args, 'ctx, 'a, 'fieldType>,
  ) => {
    let newCb = (obj, context) => {
      let newField = cb(obj, context)
      jsUnwrapVariant(newField)
    }
    let keyValue = jsGetObjValueByKey(devObj, key)
    switch keyValue->Js.Undefined.toOption {
    | Some(_) => Js.Exn.raiseError(`Field "${key}" already exists`)
    | None => ()
    }
    let newObj = jsCreateObj(key, newCb)
    Js.Obj.assign(devObj, newObj)
  }

  let fieldInCallbackToResolver = (fields: Js.t<'a>, obj, context) => {
    let dict = Js.Dict.empty()
    fields
    ->Js.Obj.keys
    ->Js.Array2.forEach(key => {
      switch jsGetObjValueByKey(fields, key)->Js.Undefined.toOption {
      | Some(value) => dict->Js.Dict.set(key, value(obj, context))
      | None => ()
      }
    })
    dict
  }

  let addArg = (devObj, key: string, arg: field2<'a>) => {
    let keyValue = jsGetObjValueByKey(devObj, key)
    switch keyValue->Js.Undefined.toOption {
    | Some(_) => Js.Exn.raiseError(`Arg "${key}" already exists`)
    | None => ()
    }
    let newObj = jsCreateObj(key, arg)
    Js.Obj.assign(devObj, newObj)
  }

  let argsToField = (args: Js.t<'a>) => {
    let dict = Js.Dict.empty()
    args
    ->Js.Obj.keys
    ->Js.Array2.forEach(key => {
      switch jsGetObjValueByKey(args, key)->Js.Undefined.toOption {
      | Some(value) => dict->Js.Dict.set(key, value)
      | None => ()
      }
    })
    dict
  }

  let addField = (devObj, key: string, field: type_<'source, 'args, 'ctx, 'a, 'fieldType>) => {
    let keyValue = jsGetObjValueByKey(devObj, key)
    switch keyValue->Js.Undefined.toOption {
    | Some(_) => Js.Exn.raiseError(`Field "${key}" already exists`)
    | None => ()
    }
    let field = jsUnwrapVariant(field)
    let newObj = jsCreateObj(key, field)
    Js.Obj.assign(devObj, newObj)
  }

  let fieldToResolver = argsToField
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
