{-

	haskell-indexing - Group oriented programming in Haskell on indicies
	BSD license.
	by Sven Nilsen, 2013
	http://www.cutoutpro.com

	Version: 0.000 in angular degrees version notation
	http://isprogrammingeasy.blogspot.no/2012/08/angular-degrees-versioning-notation.html

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
1. Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright notice,
this list of conditions and the following disclaimer in the documentation
and/or other materials provided with the distribution.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
The views and conclusions contained in the software and documentation are those
of the authors and should not be interpreted as representing official policies,
either expressed or implied, of the FreeBSD Project.

-}

-- This function takes a list of lists and removes empty lists.
indexing_Strip ::            [[a]] -> [[a]]
indexing_Strip [] =          []
indexing_Strip ([]:b) =      (indexing_Strip b)
indexing_Strip (a:b) =       a:(indexing_Strip b)

-- This function takes two lists of indices and joins them.
-- Duplicates are removed.
indexing_Or [] [] = []
indexing_Or a [] = a
indexing_Or [] a = a
indexing_Or (a0:a1) (b0:b1)
    | a0 < b0 = a0:(indexing_Or a1 (b0:b1))
    | b0 < a0 = b0:(indexing_Or (a0:a1) b1)
    | otherwise = a0:(indexing_Or a1 b1)

-- This function takes two lists and returns the intersecting indices.
indexing_And _ [] = []
indexing_And [] _ = []
indexing_And (a0:a1) (b0:b1)
    | a0 < b0 = (indexing_And a1 (b0:b1))
    | b0 < a0 = (indexing_And (a0:a1) b1)
    | otherwise = a0:(indexing_And a1 b1)

-- This function takes one list of indices and excludes the indices in the other list.
indexing_Exclude a [] = a
indexing_Exclude [] a = []
indexing_Exclude (a0:a1) (b0:b1)
    | a0 < b0 = a0:(indexing_Exclude a1 (b0:b1))
    | b0 < a0 = (indexing_Exclude (a0:a1) b1)
    | otherwise = (indexing_Exclude a1 b1)
