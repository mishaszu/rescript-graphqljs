@module("node:assert/strict") external equal: ('a, 'b) => unit = "equal"

open Graphql

let fields = {
  open Graphql.Field
  empty()
  ->addField(
    "id",
    #Field2({
      type_: Types.idType,
      description: "my field id",
    }),
  )
  ->addField(
    "name",
    #Field2({
      type_: Types.stringType,
      description: "some name",
    }),
  )
  ->addField(
    "age",
    #Field2({
      type_: Types.intType,
      description: "some age",
    }),
  )
  ->make
}

let modelType = {
  open Graphql.ModelType
  {
    name: "test",
    description: Some("my model"),
    interfaces: None,
    fields,
  }->make
}

let model = {
  open Graphql.Model
  {
    type_: modelType,
    args: None,
    resolve: async (_obj, _args, _ctx) =>
      {
        "age": 42,
        "name": "test name",
        "id": "test id",
      }->Js.Null.return,
  }->make
}

let t = Js.Dict.fromArray([("test", model)])

let query = {
  open Query
  {
    name: "query",
    fields: t,
  }->make
}

let schema = {
  query: Some(query),
  mutation: None,
}->Schema.make

let query = "query { test { id name age } }"

Self.make({schema, source: query})
->Js.Promise2.then(async v => {
  let id = v["data"]["test"]["id"]
  let name = v["data"]["test"]["name"]
  let age = v["data"]["test"]["age"]

  equal(id, "test id")
  equal(name, "test name")
  equal(age, 42)
  Js.Promise2.resolve
})
->ignore
