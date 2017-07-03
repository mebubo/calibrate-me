exports.stringify = function(a) {
    return JSON.stringify(a);
}

exports.values = function(o) {
    const r = [];
    for (k in o) {
        r.push(o[k]);
    }
    return r;
}