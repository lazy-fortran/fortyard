module fy_pages
    !! The kinds of page of the application, one per line in sorted order.
    !! Their numbers mean nothing: no code compares kinds by order or keeps a
    !! number. Modules use this one without an only list, so a new page adds
    !! its one line here and imports nothing.
    implicit none
    public

    enum, bind(c)
    enumerator :: R_BLOB
    enumerator :: R_CHANGE
    enumerator :: R_CHANGES
    enumerator :: R_COMMIT
    enumerator :: R_COMMITS
    enumerator :: R_ISSUE
    enumerator :: R_ISSUES
    enumerator :: R_JOBS
    enumerator :: R_MISSING
    enumerator :: R_REPOS
    enumerator :: R_SERVER
    enumerator :: R_TOKENS
    enumerator :: R_TREE
    end enum

end module fy_pages
