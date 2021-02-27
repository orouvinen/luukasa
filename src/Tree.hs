{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor  #-}
module Tree (Tree, val, children, create, insert, delete, findBy) where

import           Data.List  (find, intersperse)
import           Data.Maybe (catMaybes)

data Tree a
    = Empty
    | Node
        { _val             :: a
        , _children        :: [Tree a]
        , uniqueAncestorId :: Int      -- ^ The sibling number of the root's direct child this node is descendant of
        , siblingId        :: Int      -- ^ Node's own sibling num (X)
        , depth            :: Int      -- ^ Node's depth (Y)
        } deriving (Functor, Foldable)


instance Eq (Tree a) where
    (==) Empty Empty    = True
    (==) _ Empty        = False
    (==) Empty _        = False
    (==) a b            =
        uniqueAncestorId a == uniqueAncestorId b &&
        siblingId a == siblingId b &&
        depth a == depth b


children :: Tree a -> [Tree a]
children = _children

val :: Tree a -> a
val = _val

create :: a -> Tree a
create x = Node
    { _val = x
    , _children = []
    , uniqueAncestorId = 0
    , siblingId = 0
    , depth = 0
    }

-- |Insert a new value into an existing tree.
-- The new node of the value will be child of the *first* such node that the parent predicate (the 2nd argument)
-- returns True. Ordering isn't guaranteed, so if there's one specific node you're after, you should make
-- your values unique, for example using some kind of Id field.
insert  :: a                    -- ^ Value to insert
        -> (a -> Bool)          -- ^ Predicate that should return True for the node value that is the parent of the new value node. The
        -> Tree a               -- ^ Root node. The parent node should be found somehwere under the root.
        -> Tree a
insert x _ Empty        = create x
insert x isParent root  =
    let pChildren = _children root
    in
        if isParent $ _val root then
            let newNode = Node
                    { _val = x
                    , _children = []
                    , uniqueAncestorId = if depth root == 0 then length pChildren else uniqueAncestorId root
                    , depth = depth root + 1
                    , siblingId = length pChildren
                    }
            in root { _children = newNode : pChildren }
        else
            root { _children = insert x isParent <$> pChildren }


-- |Deletes a node from tree. Root node can't be deleted.
delete  :: (Tree a -> Bool)     -- ^ First node for which the predicate evaluates to True is deleted.
        -> Tree a               -- ^ Root node
        -> Tree a
delete _ Empty  = Empty
delete p root =
    let pChildren = _children root
        unwanted = find p pChildren
    in case unwanted of
        Nothing         -> root { _children = delete p <$> pChildren }
        Just toDelete   ->
            let inheritedChildren = children toDelete
                ownChildren = filter (/= toDelete) pChildren
            in root { _children = ownChildren ++ inheritedChildren}


findBy :: (a -> Bool) -> Tree a -> Maybe (Tree a)
findBy p root
    | null root         = Nothing
    | p (_val root)     = Just root
    | otherwise         =
        let matches = catMaybes $ findBy p <$> _children root
        in if null matches
            then Nothing
            else Just $ head matches

-- Quick and dirty show instance for quick verification
instance Show a => Show (Tree a) where
    show node =
        show (_val node)
        ++ " Children: " ++ printChildren node
        ++ "\n"
        ++ unwords (show <$> _children node)
      where
        printChildren tree =
            unwords $ intersperse ", " $ fmap (show . _val) (_children tree)


