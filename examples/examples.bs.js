// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Graphql = require("../src/Graphql.bs.js");
var Graphql$1 = require("graphql");
var Js_promise2 = require("rescript/lib/js/js_promise2.js");
var Strict = require("node:assert/strict");

var fields = Graphql.Field.make(Graphql.Field.addField(Graphql.Field.addField(Graphql.Field.addField(Graphql.Field.empty(undefined), "id", {
                  NAME: "Field2",
                  VAL: {
                    type: Graphql$1.GraphQLID,
                    description: "my field id"
                  }
                }), "name", {
              NAME: "Field2",
              VAL: {
                type: Graphql$1.GraphQLString,
                description: "some name"
              }
            }), "age", {
          NAME: "Field2",
          VAL: {
            type: Graphql$1.GraphQLInt,
            description: "some age"
          }
        }));

var modelType = Graphql.ModelType.make({
      name: "test",
      description: "my model",
      fields: fields
    });

var model = Graphql.Model.make({
      type: modelType,
      resolve: (async function (_obj, _args, _ctx) {
          return {
                  id: "test id",
                  name: "test name",
                  age: 42
                };
        })
    });

var query = Graphql.Query.make(Graphql.Query.addField(Graphql.Query.empty("testQ"), "testM", model));

var schema = Graphql.Schema.make({
      query: query
    });

var query$1 = "query { testM { id name age } }";

Js_promise2.then(Graphql$1.graphql({
          schema: schema,
          source: query$1
        }), (async function (v) {
        var id = v.data.testM.id;
        var name = v.data.testM.name;
        var age = v.data.testM.age;
        Strict.equal(id, "test id");
        Strict.equal(name, "test name");
        Strict.equal(age, 42);
        return function (prim) {
          return Promise.resolve(prim);
        };
      }));

exports.fields = fields;
exports.modelType = modelType;
exports.model = model;
exports.schema = schema;
exports.query = query$1;
/* fields Not a pure module */
