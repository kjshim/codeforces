

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.util.Arrays;
import java.util.StringTokenizer;

public class BY2015_5 {
    static BufferedReader br;
    static StringTokenizer st;
    static PrintWriter out;

    public static String next() throws IOException {
        while (st == null || !st.hasMoreTokens())
            st = new StringTokenizer(br.readLine());
        return st.nextToken();
    }

    public static int nextInt() throws IOException {
        return Integer.parseInt(next());
    }

    static int n, m, ff, cn, bn;
    static int[] first, to, next, color, b, sz, profit, tt;
    static boolean[] used, deleted;
    static boolean[][] a;
    static boolean[][] f;


    static void addEdge(int u, int v) {
        to[ff] = v;
        next[ff] = first[u];
        first[u] = ff++;
    }

    static void dfs1(int u) {
        used[u] = true;
        for (int i = first[u]; i != -1; i = next[i]) {
            int v = to[i];
            if (!used[v]) {
                deleted[i] = true;
                dfs1(v);
            }
        }
        b[bn++] = u;
    }

    static void dfs2(int u) {
        color[u] = cn;
        ++sz[cn];
        for (int i = first[u]; i != -1; i = next[i]) if (!deleted[i]) {
            int v = to[i];
            if (color[v] == -1) {
                dfs2(v);
            }
        }
    }

    static int rec(int u, int p) {
        //out.println((u + 1) + " " + profit[u]);
        profit[u] = 0;
        int total = sz[u];
        for (int v = 0; v < cn; ++v) if (a[u][v]) {
            if (v == p) continue;
            int k = rec(v, u);
            total += k;
            profit[u] += k * sz[u];
            profit[u] += profit[v];
        }

        tt[u] = total;
        return total;
    }

    static int dfs(int u, int p, int s, int pr) {
        int res = 0;
        pr += profit[u];
        boolean[] prev = f[u];
        prev[s] = true;

        int total = s;
        for (int v = 0; v < cn; ++v) if (a[u][v]) {
            if (v == p) continue;
            res = Math.max(res, dfs(v, u, n - tt[v], pr - profit[v] - tt[v] * sz[u] + sz[v] * (n - tt[v])));

            for (int j = total; j >= 0; --j) if (prev[j]) {
                prev[j + tt[v]] = true;
            }
            total += tt[v];
        }


        //out.println( (u + 1) + " " + (p + 1) + " " + s + " " + pr);
        for (int j = 0; j <= total; ++j) if (prev[j]) {
            res = Math.max(res, j * (total - j) + pr);
        }
        return res;
    }

    public static void main(String[] args) throws IOException {
        br = new BufferedReader(new InputStreamReader(System.in));
        out = new PrintWriter(System.out);
        n = nextInt();
        m = nextInt();
        first = new int[n];
        Arrays.fill(first, -1);
        to = new int[2 * m];
        next = new int[2 * m];
        deleted = new boolean[2 * m];
        for (int i = 0; i < m; ++i) {
            int u = nextInt() - 1;
            int v = nextInt() - 1;
            addEdge(u, v);
            addEdge(v, u);
        }

        used = new boolean[n];
        b = new int[n];
        for (int i = 0; i < n; ++i) {
            if (!used[i]) {
                dfs1(i);
            }
        }

        color = new int[n];
        Arrays.fill(color, -1);
        sz = new int[n];
        int res = 0;

        for (int i = bn - 1; i >= 0; --i) {
            int u = b[i];
            if (color[u] == -1) {
                dfs2(u);
                res += sz[cn] * sz[cn];
                cn++;
            }
        }

        a = new boolean[cn][cn];
        for (int u = 0; u < n; ++u) {
            for (int i = first[u]; i != -1; i = next[i]) {
                int v = to[i];
                if (color[u] != color[v]) {
                    a[color[u]][color[v]] = true;
                }
            }
        }

        tt = new int[cn];
        profit = new int[cn];
        rec(0, -1);

        f = new boolean[cn][n + 1];
        res += dfs(0, -1, 0, 0);

        out.println(res);
        out.close();
    }

}