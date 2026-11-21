{-# LANGUAGE OverloadedRecordDot #-}

-- A statement that opens with brackets written across lines closes them a
-- step in, where at the statement's column the bracket would begin another
-- statement, whatever follows them: a signature, a type application, a
-- record update or a field.
module Elf.Headers where

parseHeader bytes = do
    ( case magic bytes of
        Valid -> pure ()
        Invalid -> fail "not an ELF file"
        )
        :: Get ()
    ( decode
        bytes
        )
        @Header
    ( header
        bytes
        )
        { entry = 0
        }
    ( section
        bytes
        ).size
