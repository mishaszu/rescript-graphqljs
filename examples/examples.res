@module("node:assert/strict") external equal: ('a, 'b) => unit = "equal"

open Graphql

type testFields = {
  id: string,
  name: string,
  age: int,
}

let fields = {
  open Graphql.Field
  empty()
  ->addField(
    "id",
    {
      type_: Types.idType,
      description: "my field id",
    }->#Field2,
  )
  ->addField(
    "name",
    {
      type_: Types.stringType,
      description: "some name",
    }->#Field2,
  )
  ->addField(
    "age",
    {
      type_: Types.intType,
      description: "some age",
    }->#Field2,
  )
  ->make
}

let modelType = {
  {
    name: "test",
    description: "my model",
    fields,
  }->ModelType.make
}

let model = {
  type_: modelType,
  resolve: async (_obj, _args, _ctx) =>
    {
      age: 42,
      name: "test name",
      id: "test id",
    }->Js.Null.return,
}->Model.make

let query = {
  open Query
  empty("testQ")->addField("testM", model)->make
}

let schema = {
  query: query,
}->Schema.make

let query = "query { testM { id name age } }"

Self.make({schema, source: query})
->Js.Promise2.then(async v => {
  let id = v["data"]["testM"]["id"]
  let name = v["data"]["testM"]["name"]
  let age = v["data"]["testM"]["age"]

  equal(id, "test id")
  equal(name, "test name")
  equal(age, 42)
  Js.Promise2.resolve
})
->ignore
