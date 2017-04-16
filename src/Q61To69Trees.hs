module Q61To69Trees where

import Q55To60 (Tree(..))

tree4::Tree Int
tree4 = Branch 1
               (Branch 2
                       Empty
                       (Branch 4 Empty Empty)
               )
               (Branch 2 Empty Empty)

tree64::Tree Char
tree64 = Branch 'n'
                (Branch 'k'
                        (Branch 'c'
                                (Branch 'a' Empty Empty)
                                (Branch 'h'
                                        (Branch 'g'
                                                (Branch 'e' Empty Empty)
                                                Empty
                                        )
                                        Empty
                                )
                        )
                        (Branch 'm' Empty Empty)
                )
                (Branch 'u'
                        (Branch 'p'
                                Empty
                                (Branch 's'
                                        (Branch 'q' Empty Empty)
                                        Empty
                                )
                        )
                        Empty
                )

tree65::Tree Char
tree65 = Branch 'n'
                (Branch 'k'
                        (Branch 'c'
                                (Branch 'a' Empty Empty)
                                (Branch 'e'
                                        (Branch 'd' Empty Empty)
                                        (Branch 'g' Empty Empty)
                                )
                        )
                        (Branch 'm' Empty Empty)
                )
                (Branch 'u'
                        (Branch 'p'
                                Empty
                                (Branch 'q' Empty Empty)
                        )
                        Empty
                )

tree68::Tree Char
tree68 = Branch 'f'
                (Branch 'b'
                        (Branch 'a' Empty Empty)
                        (Branch 'd'
                                (Branch 'c' Empty Empty)
                                (Branch 'e' Empty Empty)
                        )
                )
                (Branch 'g'
                        Empty
                        (Branch 'i'
                                (Branch 'h' Empty Empty)
                                Empty
                        )
                )
