
{-|
Module      : Data.Functor.Extras
Description : Multi-level functors (functors of functors)
Copyright   : (c) Brian Hurt, 2020
License     : BSD 3-clause
Maintainer  : bhurt42@gmail.com
Stability   : stable

It is an increasing common occurrence to encounter functors containing
functors.  An example of this would be wanting to convert an @IO (Maybe a)@
to a @IO (Maybe b)@.  You can do stunts like @fmap f \<\$\> ...@ but
this gets cumbersome and hard to understand quickly.  This module provides
the obvious multi-level functor functions- allowing us to instead write
@f \<\$\$\> ...@.

-}
module Data.Functor.Extras (
    -- * Re-export the Functor module for convience.
    module Functor

    -- * Single-level functor functions
    , fconst
    , ffor

    -- * Two-level functor functions 
    --
    -- Functors containing functors.
    --
    , fmap2
    , (<$$>)
    , fconst2
    , (<$$)
    , ($$>)
    , ffor2
    , (<&&>)
    , void2

    -- * Three level functor functions
    , fmap3
    , (<$$$>)
    , fconst3
    , (<$$$)
    , ($$$>)
    , ffor3 
    , (<&&&>)
    , void3

    -- * Four level functor functions
    , fmap4
    , (<$$$$>)
    , fconst4
    , (<$$$$)
    , ($$$$>)
    , ffor4 
    , (<&&&&>)
    , void4

) where

    import Data.Functor as Functor

    -- | Named version of the @\<\$@ operator.
    --
    -- It's easier to partially apply or flip a named function than it
    -- is an operator.
    fconst :: Functor f => a -> f b -> f a
    fconst = (<$)

    -- | Flipped fmap.
    --
    -- Usefull for cases where the function is more interesting and larger.
    ffor :: Functor f => f a -> (a -> b) -> f b
    ffor = flip fmap

    -- | Two-level fmap.
    fmap2 :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
    fmap2 = fmap . fmap

    -- | Two-level fmap operator.
    (<$$>) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
    (<$$>) = fmap2
    infixl 4 <$$>

    -- | Two-level fconst.
    --
    -- See `fconst`.
    fconst2 :: (Functor f1, Functor f2) => a -> f1 (f2 b) -> f1 (f2 a) 
    fconst2 = fmap . fconst

    -- | Two-level fconst operator.
    --
    -- Like @\<\$@ but for two levels of functors.
    (<$$) :: (Functor f1, Functor f2) => a -> f1 (f2 b) -> f1 (f2 a)
    (<$$) = fconst2
    infixl 4 <$$

    -- | Two-level flipped fconst operator.
    --
    -- Like @\$\>@ but for two levels of functors.
    ($$>) :: (Functor f1, Functor f2) => f1 (f2 b) -> a -> f1 (f2 a)
    ($$>) = flip fconst2
    infixl 4 $$>

    -- | Flipped two-level fmap.
    ffor2 :: (Functor f1, Functor f2) => f1 (f2 a) -> (a -> b) -> f1 (f2 b)
    ffor2 = flip fmap2

    -- | Late-bindinf2 two-level fmap operator.
    --
    -- Like @\<\&\>@ but for two levels of functors.
    (<&&>) :: (Functor f1, Functor f2) => f1 (f2 a) -> (a -> b) -> f1 (f2 b)
    (<&&>) = ffor2
    infixl 1 <&&>

    -- | Two-level void.
    void2 :: (Functor f1, Functor f2) => f1 (f2 a) -> f1 (f2 ())
    void2 = fmap void

    -- | Three-level fmap.
    fmap3 :: (Functor f1, Functor f2, Functor f3)
                => (a -> b) -> f1 (f2 (f3 a)) -> f1 (f2 (f3 b))
    fmap3 = fmap . fmap2

    -- | Three-level fmap operator.
    (<$$$>) :: (Functor f1, Functor f2, Functor f3)
                    => (a -> b) -> f1 (f2 (f3 a)) -> f1 (f2 (f3 b))
    (<$$$>) = fmap3
    infixl 4 <$$$>

    -- | Three-level fconst.
    --
    -- See `fconst`.
    fconst3 :: (Functor f1, Functor f2, Functor f3)
                => a -> f1 (f2 (f3 b)) -> f1 (f2 (f3 a))
    fconst3 = fmap2 . fconst

    -- | Three-level fconst operator.
    (<$$$) :: (Functor f1, Functor f2, Functor f3)
                => a -> f1 (f2 (f3 b)) -> f1 (f2 (f3 a))
    (<$$$) = fconst3
    infixl 4 <$$$

    -- | Three-level flipped fconst operator.
    ($$$>) :: (Functor f1, Functor f2, Functor f3)
                => f1 (f2 (f3 b)) -> a -> f1 (f2 (f3 a))
    ($$$>) = flip fconst3
    infixl 4 $$$>

    -- | Flipped three-level fmap.
    ffor3 :: (Functor f1, Functor f2, Functor f3)
                    => f1 (f2 (f3 a)) -> (a -> b) -> f1 (f2 (f3 b))
    ffor3 = flip fmap3

    -- | Late-binding three-level fmap operator.
    (<&&&>) :: (Functor f1, Functor f2, Functor f3)
                => f1 (f2 (f3 a)) -> (a -> b) -> f1 (f2 (f3 b))
    (<&&&>) = ffor3
    infixl 1 <&&&>

    -- | Three-level void.
    void3 :: (Functor f1, Functor f2, Functor f3)
                => f1 (f2 (f3 a)) -> f1 (f2 (f3 ()))
    void3 = fmap void2

    -- | Four-level fmap.
    fmap4 :: (Functor f1, Functor f2, Functor f3, Functor f4)
                => (a -> b) -> f1 (f2 (f3 (f4 a))) -> f1 (f2 (f3 (f4 b)))
    fmap4 = fmap . fmap3

    -- | Four-level fmap operator.
    (<$$$$>) :: (Functor f1, Functor f2, Functor f3, Functor f4)
                    => (a -> b) -> f1 (f2 (f3 (f4 a))) -> f1 (f2 (f3 (f4 b)))
    (<$$$$>) = fmap4
    infixl 4 <$$$$>

    -- | Four-level fconst.
    --
    -- See `fconst`.
    fconst4 :: (Functor f1, Functor f2, Functor f3, Functor f4)
                => a -> f1 (f2 (f3 (f4 b))) -> f1 (f2 (f3 (f4 a)))
    fconst4 = fmap3 . fconst

    -- | Four-level fconst operator.
    (<$$$$) :: (Functor f1, Functor f2, Functor f3, Functor f4)
                => a -> f1 (f2 (f3 (f4 b))) -> f1 (f2 (f3 (f4 a)))
    (<$$$$) = fconst4
    infixl 4 <$$$$

    -- | Four-level flipped fconst operator.
    ($$$$>) :: (Functor f1, Functor f2, Functor f3, Functor f4)
                => f1 (f2 (f3 (f4 b))) -> a -> f1 (f2 (f3 (f4 a)))
    ($$$$>) = flip fconst4
    infixl 4 $$$$>

    -- | Flipped four-level fmap.
    ffor4 :: (Functor f1, Functor f2, Functor f3, Functor f4)
                    => f1 (f2 (f3 (f4 a))) -> (a -> b) -> f1 (f2 (f3 (f4 b)))
    ffor4 = flip fmap4

    -- | Late-binding four-level fmap operator.
    (<&&&&>) :: (Functor f1, Functor f2, Functor f3, Functor f4)
                => f1 (f2 (f3 (f4 a))) -> (a -> b) -> f1 (f2 (f3 (f4 b)))
    (<&&&&>) = ffor4
    infixl 1 <&&&&>

    -- | Four-level void.
    void4 :: (Functor f1, Functor f2, Functor f3, Functor f4)
                => f1 (f2 (f3 (f4 a))) -> f1 (f2 (f3 (f4 ())))
    void4 = fmap void3

    -- | Five-level fmap.
    fmap5 :: (Functor f1, Functor f2, Functor f3, Functor f4, Functor f5)
                => (a -> b)
                -> f1 (f2 (f3 (f4 (f5 a))))
                -> f1 (f2 (f3 (f4 (f5 b))))
    fmap5 = fmap . fmap4

    -- | Five-level fmap operator.
    (<$$$$$>) :: (Functor f1, Functor f2, Functor f3, Functor f4, Functor f5)
                    => (a -> b)
                    -> f1 (f2 (f3 (f4 (f5 a))))
                    -> f1 (f2 (f3 (f4 (f5 b))))
    (<$$$$$>) = fmap5
    infixl 4 <$$$$$>

    -- | Five-level fconst.
    --
    -- See `fconst`.
    fconst5 :: (Functor f1, Functor f2, Functor f3, Functor f4, Functor f5)
                => a
                -> f1 (f2 (f3 (f4 (f5 b))))
                -> f1 (f2 (f3 (f4 (f5 a))))
    fconst5 = fmap4 . fconst

    -- | Five-level fconst operator.
    (<$$$$$) :: (Functor f1, Functor f2, Functor f3, Functor f4, Functor f5)
                => a
                -> f1 (f2 (f3 (f4 (f5 b))))
                -> f1 (f2 (f3 (f4 (f5 a))))
    (<$$$$$) = fconst5
    infixl 4 <$$$$$

    -- | Five-level flipped fconst operator.
    ($$$$$>) :: (Functor f1, Functor f2, Functor f3, Functor f4, Functor f5)
                => f1 (f2 (f3 (f4 (f5 b))))
                -> a
                -> f1 (f2 (f3 (f4 (f5 a))))
    ($$$$$>) = flip fconst5
    infixl 4 $$$$$>

    -- | Flipped five-level fmap.
    ffor5 :: (Functor f1, Functor f2, Functor f3, Functor f4, Functor f5)
                    => f1 (f2 (f3 (f4 (f5 a))))
                    -> (a -> b)
                    -> f1 (f2 (f3 (f4 (f5 b))))
    ffor5 = flip fmap5

    -- | Late-binding five-level fmap operator.
    (<&&&&&>) :: (Functor f1, Functor f2, Functor f3, Functor f4, Functor f5)
                => f1 (f2 (f3 (f4 (f5 a))))
                -> (a -> b)
                -> f1 (f2 (f3 (f4 (f5 b))))
    (<&&&&&>) = ffor5
    infixl 1 <&&&&&>

    -- | Five-level void.
    void5 :: (Functor f1, Functor f2, Functor f3, Functor f4, Functor f5)
                => f1 (f2 (f3 (f4 (f5 a))))
                -> f1 (f2 (f3 (f4 (f5 ()))))
    void5 = fmap void4

