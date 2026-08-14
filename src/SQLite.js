import { DatabaseSync, StatementSync } from "node:sqlite";
export const openImpl = (path) => (options) => () => new DatabaseSync(path, {
    // configurable options
    ...options
    // fixed options
    ,
    open: true // open the database immediately, no need to call db.open() later
    ,
    readBigInts: false // integer fields are read as JavaScript numbers not BigInts
    ,
    returnArrays: false // rows are returned as objects not arrays
    // allow named parameters without the leading $, or : when binding parameters, e.g. stmt.run({ id: 1 }) instead of stmt.run({ $id: 1 })
    ,
    allowBareNamedParameters: true
});
export const closeImpl = (db) => () => db.close();
export const prepareImpl = (db) => (sql) => () => db.prepare(sql);
export const runImpl = (stmt) => (params) => () => stmt.run(params);
export const execImpl = (db) => (sql) => () => db.exec(sql);
export const allImpl = (stmt) => (params) => () => stmt.all(params);
export const getImpl = (stmt) => (params) => () => stmt.get(params);
