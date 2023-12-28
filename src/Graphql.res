// for integration with graphql-relay
type interface
type connectionArgs<'a>
type outputType

type resolverInfo
type fieldNode
type schema
type fragmentDefinition
type operationDefinition

let identity: 'a => 'b = %raw("function(a) {return a}")

module ResolverInfo = {
  type t<'parent, 'variable, 'a> = {
    fieldName: string,
    fieldNodes: array<fieldNode>,
    returnType: outputType,
    parentType: 'parent,
    schema: schema,
    fragments: Js.Dict.t<fragmentDefinition>,
    rootValue: 'a,
    operation: operationDefinition,
    variableValues: Js.Dict.t<'variable>,
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
  @module("graphql") @new external listType: t => t = "GraphQLList"

  type enumValue<'a> = {value: 'a}

  type enum<'a> = {
    name: string,
    values: Js.Dict.t<enumValue<'a>>,
  }
  @module("graphql") @new external enumType: enum<'a> => t = "GraphQLEnumType"
}

module Input = {
  type v<'a> = @unwrap
  [
    | #String(string)
    | #Int(int)
    | #Float(float)
    | #Boolean(bool)
    | #Custom('a)
  ]

  type t<'a> = {
    @as("type") type_: Types.t,
    defaultValue?: v<'a>,
    description?: string,
  }

  type inputInternal

  type m = {
    @as("type") type_: Types.t,
    defaultValue: Js.undefined<inputInternal>,
    description: Js.undefined<string>,
  }

  let make = (input: t<'a>): m => {
    type_: input.type_,
    defaultValue: {
      switch input.defaultValue {
      | Some(v) =>
        switch v {
        | #String(v) => v->identity->Js.Option.some
        | #Int(v) => v->identity->Js.Option.some
        | #Float(v) => v->identity->Js.Option.some
        | #Boolean(v) => v->identity->Js.Option.some
        | #Custom(v) => v->identity->Js.Option.some
        }
      | None => None
      }->Js.Undefined.fromOption
    },
    description: input.description->Js.Undefined.fromOption,
  }

  let merge = (input1: Js.Dict.t<m>, key, input2: t<'a>): Js.Dict.t<m> => {
    let input = make(input2)
    input1->Js.Dict.set(key, input)
    input1
  }

  let mergeMany = (input1: Js.Dict.t<m>, input2: Js.Dict.t<t<'a>>): Js.Dict.t<m> => {
    input2->Js.Dict.entries->Js.Array2.reduce((accu, (key, curr)) => merge(accu, key, curr), input1)
  }
}

module Field = {
  type resolver

  type f = {
    @as("type") type_: Types.t,
    description: Js.undefined<string>,
    deprecationReason: Js.undefined<string>,
    args: Js.undefined<Js.Dict.t<Input.m>>,
    resolver: Js.undefined<resolver>,
  }

  type resolverOutput

  module Resolver = {
    // TODO: need to add ResolverInfo.t
    type t<'source, 'args, 'ctx> = ('source, 'args, 'ctx) => promise<Js.Null.t<resolverOutput>>

    let make = (r: option<('source, 'args, 'ctx) => 'a>): option<
      ('source, 'args, 'ctx) => promise<Js.Null.t<resolverOutput>>,
    > => {
      Js.Option.map((. i) => identity(i), r)
    }
  }

  type field2 = {
    @as("type") type_: Types.t,
    description: string,
    deprecationReason?: string,
  }

  type field3<'source, 'args, 'ctx> = {
    type_: Types.t,
    deprecationReason?: string,
    description: string,
    resolve?: Resolver.t<'source, 'args, 'ctx>,
  }

  type fieldFull<'source, 'args, 'ctx> = {
    type_: Types.t,
    deprecationReason?: string,
    description?: string,
    args?: Js.Dict.t<Input.m>,
    resolve?: Resolver.t<'source, 'args, 'ctx>,
  }

  type field<'source, 'args, 'ctx> = @unwrap
  [
    | #Field2(field2)
    | #Field3(field3<'source, 'args, 'ctx>)
    | #FieldFull(fieldFull<'source, 'args, 'ctx>)
  ]

  type fields<'source, 'args, 'ctx> = Js.Dict.t<field<'source, 'args, 'ctx>>

  let empty = (): fields<'source, 'args, 'ctx> => Js.Dict.empty()

  let addField = (fields, key, field: field<'source, 'args, 'ctx>): fields<
    'source,
    'args,
    'ctx,
  > => {
    fields->Js.Dict.set(key, field)
    fields
  }

  let makeField = (f: field<'source, 'args, 'ctx>): f => {
    switch f {
    | #Field2(f) => {
        type_: f.type_,
        description: f.description->Js.Undefined.return,
        deprecationReason: f.deprecationReason->Js.Undefined.fromOption,
        args: None->Js.Undefined.fromOption,
        resolver: None->Js.Undefined.fromOption,
      }

    | #Field3(f) => {
        type_: f.type_,
        description: f.description->Js.Undefined.return,
        deprecationReason: f.deprecationReason->Js.Undefined.fromOption,
        args: None->Js.Undefined.fromOption,
        resolver: f.resolve->Js.Option.map((. a) => identity(a), _)->Js.Undefined.fromOption,
      }

    | #FieldFull(f) => {
        type_: f.type_,
        description: f.description->Js.Undefined.fromOption,
        deprecationReason: f.deprecationReason->Js.Undefined.fromOption,
        args: f.args->Js.Undefined.fromOption,
        resolver: f.resolve->Js.Option.map((. a) => identity(a), _)->Js.Undefined.fromOption,
      }
    }
  }

  let make = (fields: fields<'source, 'args, 'ctx>): Js.Dict.t<f> => {
    let dict = Js.Dict.empty()
    fields
    ->Js.Dict.entries
    ->Js.Array2.forEach(((key, field)) => {
      dict->Js.Dict.set(key, makeField(field))
    })
    ->ignore
    dict
  }
}

module Object = {
  type t

  type p<'source, 'args, 'ctx, 'data> = {
    name: string,
    interfaces?: array<interface>,
    fields: Js.Dict.t<Field.f>,
    isTypeOf?: ('source, Js.undefined<resolverInfo>) => bool,
    description?: string,
  }

  @module("graphql") @new
  external make: p<'source, 'args, 'ctx, 'data> => t = "GraphQLObjectType"
}

module ModelType = {
  type m<'source, 'args, 'ctx, 'data> = {
    name: string,
    description?: string,
    interfaces?: unit => array<interface>,
    fields: Js.Dict.t<Field.f>,
  }

  module Internal = {
    type t<'source, 'args, 'ctx, 'data> = {
      name: string,
      description: Js.undefined<string>,
      interfaces: Js.undefined<unit => array<interface>>,
      fields: Js.Dict.t<Field.f>,
    }

    @module("graphql") @new
    external newGraphqlObjectType: t<'source, 'args, 'ctx, 'data> => Types.t = "GraphQLObjectType"
  }

  let make = (model: m<'source, 'args, 'ctx, 'data>) =>
    Internal.newGraphqlObjectType({
      name: model.name,
      description: model.description->Js.Undefined.fromOption,
      interfaces: model.interfaces->Js.Undefined.fromOption,
      fields: model.fields,
    })
}

module Model = {
  type m

  type resolver<'source, 'args, 'ctx, 'data> = ('source, 'args, 'ctx) => promise<Js.Null.t<'data>>

  type t<'source, 'args, 'ctx, 'data> = {
    @as("type") type_: Types.t,
    args?: Js.Dict.t<Input.m>,
    resolve: resolver<'source, 'args, 'ctx, 'data>,
  }

  let make: t<'source, 'args, 'ctx, 'data> => m = %raw("function(v) {return v}")
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
      fields: Js.Dict.t<Model.m>,
    }
    @module("graphql") @new
    external make: t => q = "GraphQLObjectType"
  }

  let make = (query: t): q =>
    Internal.make({
      name: query.name,
      fields: query.fields,
    })
}

module Mutation = {
  type m

  type t = {
    name: string,
    fields: Js.Dict.t<Model.m>,
  }

  let empty = name => {name, fields: Js.Dict.empty()}

  let addField = (mutation, key, field) => {
    mutation.fields->Js.Dict.set(key, field)
    mutation
  }

  module Internal = {
    type t = {
      name: string,
      fields: Js.Dict.t<Model.m>,
    }
    @module("graphql") @new
    external make: t => m = "GraphQLObjectType"
  }

  let make = (mutation: t): m =>
    Internal.make({
      name: mutation.name,
      fields: mutation.fields,
    })
}

module Schema = {
  type t

  type schemaConfig = {
    query: Query.q,
    mutation?: Mutation.m,
  }

  module Internal = {
    type schemaConfig__internal<'t, 'f> = {
      query: 't,
      mutation: Js.undefined<'f>,
    }

    @module("graphql") @new
    external make__internal: schemaConfig__internal<'q, 'm> => t = "GraphQLSchema"
  }

  let make = (config: schemaConfig) =>
    Internal.make__internal({
      query: config.query,
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
