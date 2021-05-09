const isEqual = (a, b) => a === b
const isLt = (a, b) => a < b
const isLe = (a, b) => a <= b
const isGt = (a, b) => a > b
const isGe = (a, b) => a >= b
// ...u.s.w. alle Vergleichsoperationen müssen extra implementiert werden auch für Arrays etc..

const expect = (a, compareFunction, b) =>
    compareFunction(a, b)
        ? "Succeeded"
        : ["Failed", a, b]



expect(toDigits(1234), isEqual, [1,2,3,4])