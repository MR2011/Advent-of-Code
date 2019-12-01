import re
import collections
input_lines = [
"#ip 4",
"addi 4 16 4",
"seti 1 8 1",
"seti 1 3 5",
"mulr 1 5 3",
"eqrr 3 2 3",
"addr 3 4 4",
"addi 4 1 4",
"addr 1 0 0",
"addi 5 1 5",
"gtrr 5 2 3",
"addr 4 3 4",
"seti 2 2 4",
"addi 1 1 1",
"gtrr 1 2 3",
"addr 3 4 4",
"seti 1 4 4",
"mulr 4 4 4",
"addi 2 2 2",
"mulr 2 2 2",
"mulr 4 2 2",
"muli 2 11 2",
"addi 3 6 3",
"mulr 3 4 3",
"addi 3 8 3",
"addr 2 3 2",
"addr 4 0 4",
"seti 0 1 4",
"setr 4 4 3",
"mulr 3 4 3",
"addr 4 3 3",
"mulr 4 3 3",
"muli 3 14 3",
"mulr 3 4 3",
"addr 2 3 2",
"seti 0 4 0",
"seti 0 7 4"
]
a,b = map(int, [re.findall('\d+', input_lines[i])[1] for i in [22, 24]])
number_to_factorize = 10551236 + a * 22 + b

factors = collections.defaultdict(lambda: 0)
possible_prime_divisor = 2
while possible_prime_divisor ** 2 <= number_to_factorize:
  while number_to_factorize % possible_prime_divisor == 0:
    number_to_factorize /= possible_prime_divisor
    factors[possible_prime_divisor] += 1 
  possible_prime_divisor += 1
if number_to_factorize > 1:
  factors[number_to_factorize] += 1

sum_of_divisors = 1
for prime_factor in factors:
  sum_of_divisors *= (prime_factor ** (factors[prime_factor] + 1) - 1) / (prime_factor - 1)

print sum_of_divisors