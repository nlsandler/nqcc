int main() {
    int a = 0;
    for (int i = 0; i < 5; i = i + 1)
        if (i % 2)
            a = a + 1;
    return a;
}