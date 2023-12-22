@module("node:assert/strict") external equal: ('a, 'b) => unit = "equal"

open Graphql

type testFields = {
  id: string,
  name: string,
  age: int,
}

let fields: Graphql.Field.t<unit, unit, unit, testFields> = {
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

let run = %raw("function(f) {return f()}")
let run2 = %raw("function(f) {return f._fields()}")

// Js.log(run(fields))

let modelType = {
  {
    name: "test",
    description: Some("my model"),
    interfaces: None,
    fields,
  }->ModelType.make
}

let model = {
  open Graphql.Model
  {
    type_: modelType,
    args: None,
    resolve: async (_obj, _args, _ctx) =>
      {
        age: 42,
        name: "test name",
        id: "test id",
      }->Js.Null.return,
  }->make
}

let t = Js.Dict.fromArray([("test", model)])

let query = {
  open Query
  empty("testQ")->addField("testM", model)->make
}

let schema = {
  query: Some(query),
  mutation: None,
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
