function typeMismatch(expected, found) {
  return new Error(`Type mismatch: expecting ${expected}, found ${found}`)
}

function builtinAdd(...arguments) {
  return arguments.reduce((accumulator, value) => accumulator + value, 0)
}

function builtinMul(...arguments) {
  return arguments.reduce((accumulator, value) => accumulator * value, 1)
}

function builtinNegate(value) {
  if (typeof(value) === "number") {
    return (- value)
  } else {
    throw typeMismatch("number", typeof(value))
  }
}

function builtinInvert(value) {
  if (typeof(value) === "number") {
    return (1 / value)
  } else {
    throw typeMismatch("number", typeof(value))
  }
}

function builtinEq(...arguments) {
  if (arguments.length === 0) {
    throw new Error('Not enough arguments for eq?: expecting at least 1, found 0')
  } else {
    let [first, ...rest] = arguments
    return rest.every(value => value === first)
  }
}
