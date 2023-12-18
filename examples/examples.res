type assert_
@module external getAssert: assert_ = "node:assert/strict"
@send external equal: (assert_, 'a, 'a) => unit = "equal"

let a = getAssert

open Graphql

let fields =
  Js.Dict.fromArray([
    ("id", #Field2({"type": Types.idType, "description": "my field id"})),
    ("name", #Field2({"type": Types.stringType, "description": "some name"})),
  ])->Field.make

Js.log(fields)

let model = {
  "name": "test",
  "description": Some("my model"),
  "interfaces": None,
  "fields": fields,
}->Model.make

Js.log(model)
