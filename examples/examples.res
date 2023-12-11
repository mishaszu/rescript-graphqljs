type assert_
@module external getAssert: assert_ = "node:assert/strict"
@send external equal: (assert_, 'a, 'a) => unit = "equal"

let a = getAssert

open Graphql

let fields =
  Field.createFields()
  ->Field.addFieldInCallback("id", (_obj, _context) =>
    {
      type_: InputTypes.stringType,
      description: "The id of the user.",
    }->Field.Field2
  )
  ->Field.addFieldInCallback("name", (_obj, _context) =>
    {
      type_: InputTypes.stringType,
      description: "The name of the user.",
    }->Field.Field2
  )
  ->Field.addFieldInCallback("email", (_obj, _context) =>
    {
      type_: InputTypes.stringType,
      description: "The email of the user.",
    }->Field.Field2
  )

let testValue = Js.Dict.fromArray([
  ("id", {"type": {"name1": "Sting"}}),
  ("email", {"type": {"name1": "Sting"}}),
  ("name", {"type": {"name1": "Sting"}}),
])

type testFields = {
  id: string,
  name: string,
  email: string,
}

// test section
type gObj = {"type": {"name": string}, "description": string}
let resolver: Js.Dict.t<gObj> = fields->Field.fieldInCallbackToResolver("t1", "t2")
let id_ = resolver->Js.Dict.unsafeGet("id")
getAssert->equal(id_["type"]["name"], "String")
getAssert->equal(id_["description"], "The id of the user.")
// test section end

type cb = (graphQlObject, string) => Js.Dict.t<testFields>

let testModel: Model.t<cb> = {
  name: "test",
  description: "some test model"->Js.Undefined.return,
  interfaces: Js.Undefined.empty,
  fields: fields->Field.fieldInCallbackToResolver,
}

let testModel2 = testModel->InputTypes.newGraphqlObjectType

let schema = Schema.make({query: testModel2->Js.Undefined.return, mutation: Js.Undefined.empty})
