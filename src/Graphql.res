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
  type fieldsCallback

  let empty: unit => fields = %raw("function() { return {} }")

  type resolver<'source, 'args, 'ctx, 'a> = ('source, 'args, 'ctx) => promise<Js.Null.t<'a>>

  type field2 = {@as("type") type_: Types.t, description: string}

  type field3<'source, 'args, 'ctx, 'a> = {
    type_: Types.t,
    description: string,
    resolve: option<resolver<'source, 'args, 'ctx, 'a>>,
  }

  type fieldFull<'source, 'args, 'ctx, 'a> = {
    type_: Types.t,
    description: option<string>,
    args: option<connectionArgs>,
    resolve: option<resolver<'source, 'args, 'ctx, 'a>>,
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

  module Internal = {
    type newField<'source, 'args, 'ctx, 'a> = [
      | #F(t<'source, 'args, 'ctx, 'a>)
      | #C(fieldCallback<'source, 'args, 'ctx, 'a>)
    ]

    type fields__inner<'source, 'args, 'ctx, 'a> = Js.Dict.t<newField<'source, 'args, 'ctx, 'a>>

    type field3__internal<'source, 'args, 'ctx, 'a> = {
      @as("type") type_: Types.t,
      description: string,
      resolve: Js.undefined<resolver<'source, 'args, 'ctx, 'a>>,
    }

    type fieldFull__internal<'source, 'args, 'ctx, 'a> = {
      @as("type") type_: Types.t,
      description: Js.undefined<string>,
      args: Js.undefined<connectionArgs>,
      resolve: Js.undefined<resolver<'source, 'args, 'ctx, 'a>>,
    }

    let makeField = (key, field: t<'source, 'args, 'ctx, 'a>) => {
      switch field {
      | #Field2(f) => jsCreateObj(key, f)
      | #Field3(f) => {
          let i: field3__internal<'source, 'args, 'ctx, 'a> = {
            type_: f.type_,
            description: f.description,
            resolve: f.resolve->Js.Undefined.fromOption,
          }
          jsCreateObj(key, i)
        }
      | #FieldFull(f) => {
          let i: fieldFull__internal<'source, 'args, 'ctx, 'a> = {
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

  let addField: (
    fields,
    string,
    t<_, _, _, _>,
  ) => fields = %raw("function(fields, key, field) { fields[key] = { 
      field: field['VAL'],
      key,
      type: 'normal'
    }; return fields }")

  let addCallbackField: (
    fields,
    string,
    fieldCallback<_, _, _, _>,
  ) => fields = %raw("function(fields, key, field) { fields[key] = {
      field,
      key,
      type: 'callback'
    }; return fields }")

  let make: fields => fieldsCallback = %raw("function(fields) { 
      // return (obj, args, ctx) => {
        const newObj = {};
        Object.entries(fields).map(([key, field]) => {
          if (field.type === 'normal') {
            newObj[key] = field.field;
          } else {
            newObj[key] = field.field(obj, args, ctx)['VAL'];
          }
        })
        return newObj;
      // }
  }")
}

module ModelType = {
  type m = {
    name: string,
    description: option<string>,
    interfaces: option<unit => array<interface>>,
    fields: Field.fieldsCallback,
  }

  module Internal = {
    type t = {
      name: string,
      description: Js.undefined<string>,
      interfaces: Js.undefined<unit => array<interface>>,
      fields: Field.fieldsCallback,
    }

    @module("graphql") @new
    external newGraphqlObjectType: t => Types.t = "GraphQLObjectType"
  }

  let make = (model: m) =>
    Internal.newGraphqlObjectType({
      name: model.name,
      description: model.description->Js.Undefined.fromOption,
      interfaces: model.interfaces->Js.Undefined.fromOption,
      fields: model.fields,
    })
}

module Model = {
  type m

  type t<'source, 'args, 'ctx, 'a> = {
    @as("type") type_: Types.t,
    args: option<connectionArgs>,
    resolve: Field.resolver<'source, 'args, 'ctx, 'a>,
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

  @module("graphql") @new
  external make: t => q = "GraphQLObjectType"
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
