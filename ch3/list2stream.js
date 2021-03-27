// 给一个有穷的无序流
// 转换为有序流

// arguments --> list --> stream

function cons(x, y) {
    return function (i) {
        return i === 0 ? x : y
    }
}

function car(z) {
    return z(0)
}

function cdr(z) {
    return z(1)
}

function isNull (z) {
    return z === null
}

function list(...args) {
    if (args.length === 0) {
        return null
    } else {
        return cons(args[0], list.apply(null, args.slice(1)))
    }
}

function listRef(items, n) {
    if (isNull(items)) {
        return null
    } else if (n === 0) {
        return car(items)
    } else {
        return listRef(cdr(items), n - 1)
    }
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

function list2stream(items) {
    if (!items || items.length === 0) {
        return null
    } else {
        return streamCons(car(items), function () {
            return list2stream(cdr(items))
        })
    }
}

function streamRef(s, n) {
    if (n === 0) {
        return streamCar(s)
    } else {
        return streamRef(streamCdr(s), n - 1)
    }
}

function maxStream(s) {
    function iter(stream, result) {
        if (typeof stream !== 'function') {
            return result
        } else if (streamCar(stream) > result) {
            return iter(streamCdr(stream), streamCar(stream))
        } else {
            return iter(streamCdr(stream), result)
        }
    }
    return iter(s, streamCar(s))
}

function streamFilter(s, pred) {
    if (isNull(s)) {
        return null
    } else if (pred(streamCar(s))) {
        return streamCons(streamCar(s), function () { return streamFilter(streamCdr(s), pred) })
    } else {
        return streamFilter(streamCdr(s), pred)
    }
}

function streamForEach(s, proc) {
    if (isNull(s)) {
        return 'done'
    } else {
        proc(streamCar(s))
        return streamForEach(streamCdr(s), proc)
    }
}

console.log(maxStream(list2stream(list(22, 312, 31, 12, 31, 444))))

function lowerStream(s) {
    if (isNull(s)) {
        return null
    } else {
        const target = maxStream(s)
        const rest = streamFilter(s, x => x !== target)
        return streamCons(target, function () { return lowerStream(rest) })
    }
}

streamForEach(lowerStream(list2stream(list(22, 312, 31, 12, 31, 444))), v => {
    console.log(v)
})

function sort(items) {
    const insert = (x, items) => {
        if (items.length === 0) {
            return [x]
        } else if (x > items[0]) {
            return [x, ...items]
        } else {
            return [items[0], ...(insert(x, items.slice(1)))]
        }
    }
    return items.length === 0 ? [] : insert(items[0], sort(items.slice(1)))
}

console.log(sort([3213, 21, 1, 3123, 12, 1, 12, 4, 5, 8]))
