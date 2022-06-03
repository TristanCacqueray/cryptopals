import std/num/int32

// Get the length of a list
fun list_length(xs: list<a>) : int
  length(xs)

fun prepend(xs: list<a>, x: a) : list<a>
  Cons(x, xs)

// Convert a byte to a list of bits
fun base_2(x: int32) : exn list<bool>
  fun go(acc, pos)
    acc.prepend(is-odd(x.rotr(pos)))
  if x > uint32(255)
    then throw ("base_2 only support int8")
    else list(0, 7).foldl([], go)

// Decode a hex char into a list of bits, encoded as bool
fun decode_hexchar(c: char) : exn int
  match c
    '0' -> 0
    '1' -> 1
    '2' -> 2
    '3' -> 3
    '4' -> 4
    '5' -> 5
    '6' -> 6
    '7' -> 7
    '8' -> 8
    '9' -> 9
    'a' -> 10
    'b' -> 11
    'c' -> 12
    'd' -> 13
    'e' -> 14
    'f' -> 15
    _   -> throw("Invalid hex char")

fun decode_hexchars(p: (char, char)) : exn int
  decode_hexchar(p.fst).int32().rotl(4).int() + decode_hexchar(p.snd)

fun group_by_2(l: list<a>) : exn list<(a, a)>
  fun go(rest, acc)
    match rest
      Cons(x, Cons(y, xs)) -> go(xs, acc.prepend((x, y)))
      Nil -> acc
      _ -> throw("Un-even list")
  go(l, []).reverse()

// Decode a hex string into a list of byte
fun decode_hex(s: string) : exn list<int>
  s.list().group_by_2().map(decode_hexchars)

// Chunk a list
fun chunks(xs: list<a>, size: int) : list<list<a>>
  val (res, rest) = xs.foldl(([], []), fn((res, acc), v) {
    match acc.list_length() == size {
      True -> (res.append([acc]), [v])
      False -> (res, acc.append([v]))
    }
  })
  res.append([rest])

// Encode one chunk
fun encode_base64_chunk(xs: list<bool>) : exn char
  val padded = match xs.list_length()
             6 -> xs
             n -> replicate(False, 6 - n).append(xs)
  match padded
     [False, False, False, False, False, False] -> 'A'
     [False, False, False, False, False,  True] -> 'B'
     [False, False, False, False,  True, False] -> 'C'
     [False, False, False, False,  True,  True] -> 'D'
     [False, False, False,  True, False, False] -> 'E'
     [False, False, False,  True, False,  True] -> 'F'
     [False, False, False,  True,  True, False] -> 'G'
     [False, False, False,  True,  True,  True] -> 'H'
     [False, False,  True, False, False, False] -> 'I'
     [False, False,  True, False, False,  True] -> 'J'
     [False, False,  True, False,  True, False] -> 'K'
     [False, False,  True, False,  True,  True] -> 'L'
     [False, False,  True,  True, False, False] -> 'M'
     [False, False,  True,  True, False,  True] -> 'N'
     [False, False,  True,  True,  True, False] -> 'O'
     [False, False,  True,  True,  True,  True] -> 'P'
     [False,  True, False, False, False, False] -> 'Q'
     [False,  True, False, False, False,  True] -> 'R'
     [False,  True, False, False,  True, False] -> 'S'
     [False,  True, False, False,  True,  True] -> 'T'
     [False,  True, False,  True, False, False] -> 'U'
     [False,  True, False,  True, False,  True] -> 'V'
     [False,  True, False,  True,  True, False] -> 'W'
     [False,  True, False,  True,  True,  True] -> 'X'
     [False,  True,  True, False, False, False] -> 'Y'
     [False,  True,  True, False, False,  True] -> 'Z'
     [False,  True,  True, False,  True, False] -> 'a'
     [False,  True,  True, False,  True,  True] -> 'b'
     [False,  True,  True,  True, False, False] -> 'c'
     [False,  True,  True,  True, False,  True] -> 'd'
     [False,  True,  True,  True,  True, False] -> 'e'
     [False,  True,  True,  True,  True,  True] -> 'f'
     [ True, False, False, False, False, False] -> 'g'
     [ True, False, False, False, False,  True] -> 'h'
     [ True, False, False, False,  True, False] -> 'i'
     [ True, False, False, False,  True,  True] -> 'j'
     [ True, False, False,  True, False, False] -> 'k'
     [ True, False, False,  True, False,  True] -> 'l'
     [ True, False, False,  True,  True, False] -> 'm'
     [ True, False, False,  True,  True,  True] -> 'n'
     [ True, False,  True, False, False, False] -> 'o'
     [ True, False,  True, False, False,  True] -> 'p'
     [ True, False,  True, False,  True, False] -> 'q'
     [ True, False,  True, False,  True,  True] -> 'r'
     [ True, False,  True,  True, False, False] -> 's'
     [ True, False,  True,  True, False,  True] -> 't'
     [ True, False,  True,  True,  True, False] -> 'u'
     [ True, False,  True,  True,  True,  True] -> 'v'
     [ True,  True, False, False, False, False] -> 'w'
     [ True,  True, False, False, False,  True] -> 'x'
     [ True,  True, False, False,  True, False] -> 'y'
     [ True,  True, False, False,  True,  True] -> 'z'
     [ True,  True, False,  True, False, False] -> '0'
     [ True,  True, False,  True, False,  True] -> '1'
     [ True,  True, False,  True,  True, False] -> '2'
     [ True,  True, False,  True,  True,  True] -> '3'
     [ True,  True,  True, False, False, False] -> '4'
     [ True,  True,  True, False, False,  True] -> '5'
     [ True,  True,  True, False,  True, False] -> '6'
     [ True,  True,  True, False,  True,  True] -> '7'
     [ True,  True,  True,  True, False, False] -> '8'
     [ True,  True,  True,  True, False,  True] -> '9'
     [ True,  True,  True,  True,  True, False] -> '+'
     [ True,  True,  True,  True,  True,  True] -> '/'
     _ -> throw("Chunk size too big")

fun encode_base64(xs: list<list<bool>>) : exn list<char>
  xs.map(encode_base64_chunk)

fun solve_challenge1(s: string) : exn string
  s.decode_hex().map(int32).flatmap(base_2).chunks(6).encode_base64().string()

fun test_challenge1() : <console,exn> ()
  val input = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
  val expected = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"
  val got = solve_challenge1(input)
  match expected == got
    False -> {
       println("Bad output:")
       println(got)
    }
    True -> println("Succeed!")

// Debug helpers
fun show_bin(xs: list<bool>) : string
  xs.map(fn(c) match c
    True -> '1'
    False -> '0'
  ).string()

fun show_bins(xs: list<list<bool>>) : string
  xs.map(show_bin).join(",")
