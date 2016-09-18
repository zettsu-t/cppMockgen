#!/usr/bin/ruby
# -*- coding: utf-8 -*-
#
# Common classes

# String operations
module MockgenFeatures
  attr_accessor :mockGenTestingException, :ignoreAllExceptions
  @@mockGenTestingException = false
  @@ignoreAllExceptions = false
end

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

    # Remove unrelated trailing sentence
    class ChompAfterDelimiter
      attr_reader :str, :tailStr

      def initialize(argStr, delimiter, excludedStr)
        pos = nil
        unless argStr.empty?
          pos = 0
          while(pos < argStr.size)
            pos = argStr.index(delimiter, pos)
            break if pos.nil?
            break if excludedStr.empty?

            # split with ":", not with "::"
            endPos = [pos + excludedStr.size, argStr.size].min - 1
            break if argStr[pos..endPos] != excludedStr
            pos = endPos + 1
          end
        end

        @str = pos ? argStr[0..(pos-1)].rstrip : argStr.dup
        @tailStr = pos ? argStr[pos..-1] : nil
      end
    end

    # Split a string with top-level brackets
    class StringOfBrackets
      def initialize(line)
        @line = line
      end

      def parse(splitRegex, spaceRegex)
        # Recursive regular expresstion to split () (()) () ...
        pattern = Regexp.new(splitRegex)
        phraseSet = @line.gsub(/\(/, ' (').gsub(/\)/, ') ').gsub(/\s+/, ' ').scan(pattern)

        # Remove spaces between parenthesis
        spacePattern = Regexp.new(spaceRegex)
        phraseSet.map do |phrase, captured|
          left = phrase ? phrase.gsub(spacePattern, '\1') : nil
          right = captured ? captured.gsub(spacePattern, '\1')[1..-2] : nil
          [left, right]
        end
      end
    end

    # Split "result( *f )( arg )" into [["result", nil], ["(*f)", "*f"], ["(arg)", "arg"]]
    class StringOfParenthesis
      def initialize(line)
        @line = line
        @pattern = '((?>[^\s(]+|(\((?>[^()]+|\g<-1>)*\)))+)'
      end

      def parse
        StringOfBrackets.new(@line).parse(@pattern, '\s*(\(|\))\s*')
      end

      def parseAndKeepSpaces
        StringOfBrackets.new(@line).parse(@pattern, '(\s*\(|\)\s*)').map do |elements|
          elements.map { |element| element.nil? ? nil : element.strip }
        end
      end
    end

    # Split "template <> class Name<T>" into
    #   [["template ", nil], ["<>", ""], [" class Name", nil], ["<T>", "T"]]
    # and "template <typename T, typename U = V<S>>" into
    #   [["template ", nil], ["<typename T, typename U = V<S>>", "typename T, typename U = V<S>"]]
    class StringOfAngleBrackets
      def initialize(line)
        # split >> into "> >"
        @line = line.gsub(/([<>])/, ' \1 ')
      end

      def parse
        pattern = '((?>[^\s<]+|(<(?>[^<>]+|\g<-1>)*>))+)'
        StringOfBrackets.new(@line).parse(pattern, '\s*(<|>)\s*')
      end
    end
  end
end
