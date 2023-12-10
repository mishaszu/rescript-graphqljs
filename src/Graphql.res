type t

type graphQLString
type graphQlObject

// for integration with graphql-relay
type interface
type connectionArgs

module Field = {
  type t
  type mutationField

  type simpleField<'a> = {"type": t, "description": string}

  type field<'a> = {
    "type": t,
    "description": string,
    "resolve": Js.undefined<graphQlObject => Js.Null.t<'a>>,
  }

  type fieldWithArgs<'a, 'b, 'c> = {
    "type": t,
    "description": Js.undefined<string>,
    "args": 'b,
    "resolve": (graphQlObject, 'c) => promise<Js.Null.t<'a>>,
  }
}

module Input = {
  type t<'t, 'c> = {
    name: string,
    description: Js.undefined<string>,
    interfaces: Js.undefined<unit => array<interface>>,
    fields: 'c => 't,
  }
}

module Query = {
  type graphqlQueryObject<'t> = {
    name: string,
    fields: unit => 't,
  }
}

module Mutation = {
  type mutation<'t, 'd> = {
    inputFields: 't,
    outputFields: 'd,
    mutateAndGetPayload: 't => 'd,
  }
}

module Schema = {
  type t

  type schemaConfig<'t, 'f> = {
    query: Js.undefined<'t>,
    mutation: Js.undefined<'f>,
  }

  @module("graphql") @new
  external newGraphqlSchema: schemaConfig<'q, 'm> => t = "GraphQLSchema"
}

module GraphqlTypes = {
  @module("graphql") @new
  external newGraphqlObjectType: Input.t<'t, 'c> => graphQlObject = "GraphQLObjectType"

  @module("graphql") external stringType: Field.t = "GraphQLString"
  @module("graphql") external intType: Field.t = "GraphQLInt"
  @module("graphql") external floatType: Field.t = "GraphQLFloat"
  @module("graphql") external idType: Field.t = "GraphQLID"
  @module("graphql") external booleanType: Field.t = "GraphQLBoolean"

  @module("graphql") @new external newGraphQLNonNull: Field.t => Field.t = "GraphQLNonNull"
}
