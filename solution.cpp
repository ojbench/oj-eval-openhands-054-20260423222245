#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <cstdlib>

using namespace std;

// Term: a * x^b * sin^c(x) * cos^d(x)
struct Term {
    int a, b, c, d;
    
    Term(int _a = 0, int _b = 0, int _c = 0, int _d = 0) 
        : a(_a), b(_b), c(_c), d(_d) {}
    
    bool operator==(const Term& other) const {
        return b == other.b && c == other.c && d == other.d;
    }
    
    bool operator<(const Term& other) const {
        if (b != other.b) return b > other.b;
        if (c != other.c) return c > other.c;
        return d > other.d;
    }
};

// Polynomial: sum of terms
struct Poly {
    vector<Term> terms;
    
    Poly() {}
    Poly(const vector<Term>& t) : terms(t) {}
    Poly(const Term& t) { terms.push_back(t); }
    
    void simplify() {
        if (terms.empty()) return;
        
        sort(terms.begin(), terms.end());
        
        vector<Term> result;
        for (const auto& t : terms) {
            if (t.a == 0) continue;
            if (!result.empty() && result.back() == t) {
                result.back().a += t.a;
            } else {
                result.push_back(t);
            }
        }
        
        // Remove zero terms
        result.erase(remove_if(result.begin(), result.end(), 
            [](const Term& t) { return t.a == 0; }), result.end());
        
        terms = result;
    }
    
    Poly operator+(const Poly& other) const {
        Poly result;
        result.terms = terms;
        result.terms.insert(result.terms.end(), other.terms.begin(), other.terms.end());
        result.simplify();
        return result;
    }
    
    Poly operator-(const Poly& other) const {
        Poly result;
        result.terms = terms;
        for (const auto& t : other.terms) {
            result.terms.push_back(Term(-t.a, t.b, t.c, t.d));
        }
        result.simplify();
        return result;
    }
    
    Poly operator*(const Poly& other) const {
        Poly result;
        for (const auto& t1 : terms) {
            for (const auto& t2 : other.terms) {
                result.terms.push_back(Term(
                    t1.a * t2.a,
                    t1.b + t2.b,
                    t1.c + t2.c,
                    t1.d + t2.d
                ));
            }
        }
        result.simplify();
        return result;
    }
    
    Poly derivate() const {
        Poly result;
        for (const auto& t : terms) {
            // (a * x^b * sin^c(x) * cos^d(x))'
            // = a * [(x^b)' * sin^c(x) * cos^d(x) + x^b * (sin^c(x))' * cos^d(x) + x^b * sin^c(x) * (cos^d(x))']
            
            // (x^b)' = b * x^(b-1)
            if (t.b > 0) {
                result.terms.push_back(Term(t.a * t.b, t.b - 1, t.c, t.d));
            }
            
            // (sin^c(x))' = c * sin^(c-1)(x) * cos(x)
            if (t.c > 0) {
                result.terms.push_back(Term(t.a * t.c, t.b, t.c - 1, t.d + 1));
            }
            
            // (cos^d(x))' = -d * sin(x) * cos^(d-1)(x)
            if (t.d > 0) {
                result.terms.push_back(Term(-t.a * t.d, t.b, t.c + 1, t.d - 1));
            }
        }
        result.simplify();
        return result;
    }
};

// Fraction: p / q
struct Frac {
    Poly p, q;
    
    Frac() {
        q.terms.push_back(Term(1, 0, 0, 0));
    }
    
    Frac(int val) {
        p.terms.push_back(Term(val, 0, 0, 0));
        q.terms.push_back(Term(1, 0, 0, 0));
    }
    
    Frac(const Term& t) {
        p.terms.push_back(t);
        q.terms.push_back(Term(1, 0, 0, 0));
    }
    
    Frac(const Poly& _p, const Poly& _q) : p(_p), q(_q) {}
    
    Frac operator+(const Frac& other) const {
        // p1/q1 + p2/q2 = (p1*q2 + p2*q1) / (q1*q2)
        return Frac(p * other.q + other.p * q, q * other.q);
    }
    
    Frac operator-(const Frac& other) const {
        // p1/q1 - p2/q2 = (p1*q2 - p2*q1) / (q1*q2)
        return Frac(p * other.q - other.p * q, q * other.q);
    }
    
    Frac operator*(const Frac& other) const {
        // p1/q1 * p2/q2 = (p1*p2) / (q1*q2)
        return Frac(p * other.p, q * other.q);
    }
    
    Frac operator/(const Frac& other) const {
        // p1/q1 / p2/q2 = (p1*q2) / (q1*p2)
        return Frac(p * other.q, q * other.p);
    }
    
    Frac derivate() const {
        // (p/q)' = (p'*q - q'*p) / (q*q)
        Poly dp = p.derivate();
        Poly dq = q.derivate();
        return Frac(dp * q - dq * p, q * q);
    }
    
    void output() const {
        // Check if numerator is zero
        if (p.terms.empty()) {
            cout << "0" << endl;
            return;
        }
        
        // Check if denominator is 1
        bool denom_is_one = (q.terms.size() == 1 && q.terms[0].a == 1 && 
                            q.terms[0].b == 0 && q.terms[0].c == 0 && q.terms[0].d == 0);
        
        auto output_poly = [](const Poly& poly) {
            for (size_t i = 0; i < poly.terms.size(); i++) {
                const Term& t = poly.terms[i];
                
                // Output sign and coefficient
                if (i == 0) {
                    if (t.a < 0) cout << "-";
                } else {
                    cout << (t.a > 0 ? "+" : "-");
                }
                
                int abs_a = abs(t.a);
                bool is_const = (t.b == 0 && t.c == 0 && t.d == 0);
                
                if (abs_a != 1 || is_const) {
                    cout << abs_a;
                }
                
                // Output x^b
                if (t.b > 0) {
                    cout << "x";
                    if (t.b > 1) cout << "^" << t.b;
                }
                
                // Output sin^c(x)
                if (t.c > 0) {
                    cout << "sin";
                    if (t.c > 1) cout << "^" << t.c;
                    cout << "x";
                }
                
                // Output cos^d(x)
                if (t.d > 0) {
                    cout << "cos";
                    if (t.d > 1) cout << "^" << t.d;
                    cout << "x";
                }
            }
        };
        
        if (denom_is_one) {
            output_poly(p);
            cout << endl;
        } else {
            if (p.terms.size() > 1) cout << "(";
            output_poly(p);
            if (p.terms.size() > 1) cout << ")";
            cout << "/";
            if (q.terms.size() > 1) cout << "(";
            output_poly(q);
            if (q.terms.size() > 1) cout << ")";
            cout << endl;
        }
    }
};

// Parse integer from string
int get_number(const string& s, int l, int r) {
    if (l >= r) return 1;
    
    int sign = 1;
    int start = l;
    if (s[l] == '-') {
        sign = -1;
        start++;
    }
    
    if (start >= r) return sign;
    
    int num = 0;
    bool has_digit = false;
    for (int i = start; i < r; i++) {
        if (isdigit(s[i])) {
            num = num * 10 + (s[i] - '0');
            has_digit = true;
        } else {
            break;
        }
    }
    
    if (!has_digit) return sign;
    return sign * num;
}

// Parse a term from string
Term get_term(const string& s, int l, int r) {
    int a = 0, b = 0, c = 0, d = 0;
    
    // Find coefficient
    int i = l;
    if (i < r && (s[i] == '-' || s[i] == '+')) i++;
    while (i < r && isdigit(s[i])) i++;
    
    a = get_number(s, l, i);
    
    // Parse x, sin, cos
    while (i < r) {
        if (s[i] == 'x') {
            i++;
            if (i < r && s[i] == '^') {
                i++;
                int start = i;
                while (i < r && isdigit(s[i])) i++;
                b = atoi(s.substr(start, i - start).c_str());
            } else {
                b = 1;
            }
        } else if (i + 2 < r && s.substr(i, 3) == "sin") {
            i += 3;
            if (i < r && s[i] == '^') {
                i++;
                int start = i;
                while (i < r && isdigit(s[i])) i++;
                c = atoi(s.substr(start, i - start).c_str());
                i++; // skip 'x'
            } else {
                c = 1;
                i++; // skip 'x'
            }
        } else if (i + 2 < r && s.substr(i, 3) == "cos") {
            i += 3;
            if (i < r && s[i] == '^') {
                i++;
                int start = i;
                while (i < r && isdigit(s[i])) i++;
                d = atoi(s.substr(start, i - start).c_str());
                i++; // skip 'x'
            } else {
                d = 1;
                i++; // skip 'x'
            }
        } else {
            i++;
        }
    }
    
    return Term(a, b, c, d);
}

// Forward declaration
Frac dfs(const string& s, int l, int r);

// Parse expression recursively
Frac dfs(const string& s, int l, int r) {
    // Remove outer spaces
    while (l < r && s[l] == ' ') l++;
    while (l < r && s[r-1] == ' ') r--;
    
    if (l >= r) return Frac(0);
    
    // Remove outer parentheses
    if (s[l] == '(' && s[r-1] == ')') {
        int depth = 0;
        bool can_remove = true;
        for (int i = l; i < r; i++) {
            if (s[i] == '(') depth++;
            else if (s[i] == ')') depth--;
            if (depth == 0 && i < r - 1) {
                can_remove = false;
                break;
            }
        }
        if (can_remove) return dfs(s, l + 1, r - 1);
    }
    
    // Find operators at depth 0 (+ and -)
    int depth = 0;
    int last_add_sub = -1;
    for (int i = l; i < r; i++) {
        if (s[i] == '(') depth++;
        else if (s[i] == ')') depth--;
        else if (depth == 0 && (s[i] == '+' || s[i] == '-')) {
            if (i > l) {  // Not at the beginning
                last_add_sub = i;
            }
        }
    }
    
    if (last_add_sub != -1) {
        Frac left = dfs(s, l, last_add_sub);
        Frac right = dfs(s, last_add_sub + 1, r);
        if (s[last_add_sub] == '+') {
            return left + right;
        } else {
            return left - right;
        }
    }
    
    // Find operators at depth 0 (* and /)
    depth = 0;
    int last_mul_div = -1;
    for (int i = l; i < r; i++) {
        if (s[i] == '(') depth++;
        else if (s[i] == ')') depth--;
        else if (depth == 0 && (s[i] == '*' || s[i] == '/')) {
            last_mul_div = i;
        }
    }
    
    if (last_mul_div != -1) {
        Frac left = dfs(s, l, last_mul_div);
        Frac right = dfs(s, last_mul_div + 1, r);
        if (s[last_mul_div] == '*') {
            return left * right;
        } else {
            return left / right;
        }
    }
    
    // It's a term
    Term t = get_term(s, l, r);
    return Frac(t);
}

void solve(const string& s) {
    Frac f = dfs(s, 0, s.length());
    f.output();
    Frac df = f.derivate();
    df.output();
}

int main() {
    string expr;
    getline(cin, expr);
    solve(expr);
    return 0;
}
