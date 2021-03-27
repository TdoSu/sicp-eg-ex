// 写一写状态机

let tokens = []
generateTokens('31231+312312 - 32123+312*213')
console.log(evaluation(middle2suffix(tokens)))

function generateTokens(text) {
    const chars = Array.from(text)
    const char = chars[0]
    if ('0123456789'.includes(char)) {
        numberToken(chars)
    } else if ('+-*/'.includes(char)) {
        operatorToken(chars)
    } else {
        throw new Error('未知的符号', char)
    }
}

function numberToken(chars) {
    let token = ''
    let char = ''
    while (char = chars.shift()) {
        if ('0123456789'.includes(char)) {
            token += char
        } else if ('+-*/'.includes(char)) {
            tokens.push(token)
            return operatorToken([char, ...chars])
        } else if (char === ' ') {
            tokens.push(token)
            return emptyToken([char, ...chars])
        } else {
            throw new Error('未知的符号', char)
        }
    }
    tokens.push(token)
}

function operatorToken(chars) {
    let token = ''
    let char = ''
    while (char = chars.shift()) {
        if ('0123456789'.includes(char)) {
            tokens.push(token)
            return numberToken([char, ...chars])
        } else if ('+-*/'.includes(char)) {
            token += char
        } else if (char === ' ') {
            tokens.push(token)
            return emptyToken([char, ...chars])
        } else {
            throw new Error('未知的符号', char)
        }
    }
    tokens.push(token)
}

function emptyToken(chars) {
    let token = ''
    let char = ''
    while (char = chars.shift()) {
        if ('0123456789'.includes(char)) {
            tokens.push(token)
            return numberToken([char, ...chars])
        } else if ('+-*/'.includes(char)) {
            tokens.push(token)
            return operatorToken([char, ...chars])
        } else if (char === ' ') {
            token += char
        } else {
            throw new Error('未知的符号', char)
        }
    }
    tokens.push(token)
}

function middle2suffix(list) {
    const stack = []
    const result = []
    list.forEach(v => {
        if (/\d+/.test(v)) {
            result.push(v)
        } else if ('+-*/'.includes(v)) {
            const operator = stack.pop()
            operator && result.push(operator)
            stack.push(v)
        }
    })
    const operator = stack.pop()
    operator && result.push(operator)
    return result
}

function evaluation(list) {
    const stack = []
    list.forEach(v => {
        if (/\d+/.test(v)) {
            stack.push(v)
        } else if ('+-*/'.includes(v)) {
            const v1 = stack.pop()
            const v2 = stack.pop()
            const r = eval(v2 + v + v1)
            stack.push(r)
        }
    })
    return stack.pop()
}
