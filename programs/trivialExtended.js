function a (x) {
    function c (z) {
        return z;
    }
    c(x);
}

function b (y) {
    return a(y);
}

b(1);
