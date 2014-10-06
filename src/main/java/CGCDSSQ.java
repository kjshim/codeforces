import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;

public class CGCDSSQ {
    static int N;
    static int[] A;

    static int gcd(int a, int b) {
        return (b==0) ? a : gcd(b, a%b);
    }

    public static void main(String[] args) throws IOException {
        InputReader reader = new InputReader(System.in);
        N = reader.readInt();
        A = new int[N];
        for (int n=0; n<N; n++) {
            A[n] = reader.readInt();
        }
        Map<Integer,Long> all = new HashMap<Integer,Long>();
        Map<Integer,Long> left = new HashMap<Integer,Long>();
        for (int n=0; n<N; n++) {
            int value = A[n];
            Map<Integer,Long> newLeft = new HashMap<Integer,Long>();
            for (Map.Entry<Integer,Long> entry : left.entrySet()) {
                int number = entry.getKey();
                long count = entry.getValue();
                int gcd = gcd(number, value);
                add(newLeft, gcd, count);
                add(all, gcd, count);
            }
            add(all, value, 1);
            add(newLeft, value, 1);
            left = newLeft;
        }
        int Q = reader.readInt();
        StringBuilder output = new StringBuilder(10*Q);
        for (int q=0; q<Q; q++) {
            int x = reader.readInt();
            Long result = all.get(x);
            if (result == null) {
                result = 0L;
            }
            output.append(result).append("\n");
        }
        System.out.print(output);
    }

    static void add(Map<Integer,Long> map, int value, long diff) {
        Long count = map.get(value);
        if (count == null) {
            count = diff;
        } else {
            count += diff;
        }
        map.put(value, count);
    }


    static final class InputReader {
        private final InputStream stream;
        private final byte[] buf = new byte[1024];
        private int curChar;
        private int numChars;

        public InputReader(InputStream stream) {
            this.stream = stream;
        }

        private int read() throws IOException {
            if (curChar >= numChars) {
                curChar = 0;
                numChars = stream.read(buf);
                if (numChars <= 0) {
                    return -1;
                }
            }
            return buf[curChar++];
        }

        public final int readInt() throws IOException {
            int c = read();
            while (isSpaceChar(c)) {
                c = read();
            }
            int res = 0;
            do {
                res *= 10;
                res += c - '0';
                c = read();
            } while (!isSpaceChar(c));
            return res;
        }

        private boolean isSpaceChar(int c) {
            return c == ' ' || c == '\n' || c == '\r' || c == '\t' || c == -1;
        }
    }

}