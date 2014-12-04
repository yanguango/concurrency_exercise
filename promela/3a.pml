byte n = 0;
byte finish = 0;

active [2] proctype increment1toN() {
    byte N = 5;
    byte counter = 0;
    byte temp;
    do
    :: counter >= N -> break
    :: else -> 
        atomic {
            temp = n;
            n = temp + 1;
            counter++
        }
    od;
    finish++
}

active proctype WaitForFinish() {
    finish == 2;
    assert(n == 10)
}



