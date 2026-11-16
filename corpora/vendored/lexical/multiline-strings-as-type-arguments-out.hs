{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultilineStrings #-}

module Shell.Banner where

type Banner =
  """
  Welcome to the shell.
    Type help to begin."""

farewell :: String
farewell =
  render
    @"""Goodbye,
     and thank you.
     """

greeting :: String
greeting =
  render @"Hello"
    ++ render
      @"""
       Welcome back."""
