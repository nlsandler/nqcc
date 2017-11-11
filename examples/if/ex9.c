int main() {

    int a = 2;

    if (1) {
        a = 5;
    } else {
        a = 7;
    }

    int b = a-5;

    if (b) {
        return 4;
    }

    return 5;
}