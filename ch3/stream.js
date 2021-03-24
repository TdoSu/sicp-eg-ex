function cons(a, b) {
    return i => i === 0 ? a : b
}

function car(z) {
    return z(0)
}

function cdr(z) {
    return z(1)
}

function streamCons(x, y) {
    return cons(x, y)
}

function streamCar(s) {
    return car(s)
}

function streamCdr(s) {
    return cdr(s)()
}

function streamMap(s, proc) {
    if (s === null) {
        return null
    } else {
        return streamCons(proc(streamCar(s)), function () {
            return streamMap(streamCdr(s), proc)
        })
    }
}

function streamFilter(s, pred) {
    if (s === null) {
        return null
    } else if (pred(streamCar(s))) {
        return streamCons(streamCar(s), function () {
            return streamFilter(streamCdr(s), pred)
        })
    } else {
        return streamFilter(streamCdr(s), pred)
    }
}

function streamRef(s, n) {
    if (n === 0) {
        return streamCar(s, n)
    } else {
        return streamRef(streamCdr(s), n - 1)
    }
}

function integersFromN(n) {
    return streamCons(n, function() { return integersFromN(n + 1) })
}

const integers = integersFromN(1)

console.log(streamRef(streamFilter(streamMap(integers, x => x * 2), x => x % 5 === 0), 4))

function sieve(stream) {
    return streamCons(streamCar(stream), function () {
        return streamFilter(sieve(streamCdr(stream)), x => x % (streamCar(stream)) !== 0)
    })
}

const primes = sieve(integersFromN(2))

let n = 0

setInterval(() => {
    console.log(streamRef(primes, n))
    n++
}, 500)

