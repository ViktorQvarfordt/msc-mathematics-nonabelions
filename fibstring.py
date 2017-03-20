from itertools import product

def fibstring(n):
    if n <= 1:
        return ['1', 'τ']
    else:
        res = []
        for s in fibstring(n-1):
            if s[-1] == 'τ':
                res.append(s + '1')
            res.append(s + 'τ')
        return res

def f(n):
    return sorted(fibstring(n), key=lambda s: [s[0], s[-1]])

print('\n'.join(map(lambda x: ''.join(x), product(f(2), repeat=2))))
print()
print('\n'.join(f(5)))
