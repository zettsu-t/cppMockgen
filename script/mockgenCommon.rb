#!/usr/bin/ruby
# -*- coding: utf-8 -*-
#
# Common classes

module Mockgen
  module Common
    # Remove CRLF on Cygwin and MinGW
    class LineWithoutCRLF
      attr_reader :line

      def initialize(line)
        # If it does not works, use tr("\r\n", "")
        @line = line.chomp
      end
    end
  end
end
