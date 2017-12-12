int incr(int a) {
    return a + 1;
}

int main() {
    int a = 2;
    a = (1+2) + incr(a+1);
    return a;
}