#!/usr/bin/ruby
# -*- coding: utf-8 -*-
#
# Common classes

# String operations
module MockgenFeatures
  attr_accessor :mockGenTestingException, :ignoreAllExceptions, :suppressWarnings, :useLiteralRegexp
  @@mockGenTestingException = false
  @@ignoreAllExceptions = false
  @@suppressWarnings = false
  @@useLiteralRegexp = true
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

      # should use literal regular expressions to parse fast
      def parse(splitRegex, spaceRegex)
        # Recursive regular expresstion to split () (()) () ...
        pattern = Regexp.new(splitRegex)
        phraseSet = @line.gsub("(", " (").gsub(")", ") ").gsub(/\s+/, ' ').scan(pattern)

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
      end

      def parse
        # same as StringOfBrackets.new(@line).parse
        # Recursive regular expresstion to split () (()) () ...
        phraseSet = @line.gsub("(", " (").gsub(")", ") ").gsub(/\s+/, ' ').
                    scan(/((?>[^\s(]+|(\((?>[^()]+|\g<-1>)*\)))+)/)

        # Remove spaces between parenthesis
        phraseSet.map do |phrase, captured|
          left = phrase ? phrase.gsub(/\s*(\(|\))\s*/, '\1') : nil
          right = captured ? captured.gsub(/\s*(\(|\))\s*/, '\1')[1..-2] : nil
          [left, right]
        end
      end

      def parseAndKeepSpaces
        # same as StringOfBrackets.new(@line).parse
        # Recursive regular expresstion to split () (()) () ...
        phraseSet = @line.gsub("(", " (").gsub(")", ") ").gsub(/\s+/, " ").
                    scan(/((?>[^\s(]+|(\((?>[^()]+|\g<-1>)*\)))+)/)

        # Remove spaces between parenthesis
        phraseSet.map do |phrase, captured|
          left = phrase ? phrase.gsub(/(\s*\(|\)\s*)/, '\1') : nil
          right = captured ? captured.gsub(/(\s*\(|\)\s*)/, '\1')[1..-2] : nil
          [left, right]
        end.map do |elements|
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
        # same as StringOfBrackets.new(@line).parse
        # Recursive regular expresstion to split () (()) () ...
        phraseSet = @line.gsub("(", " (").gsub(")", ") ").gsub(/\s+/, " ").
                    scan(/((?>[^\s<]+|(<(?>[^<>]+|\g<-1>)*>))+)/)

        # Remove spaces between parenthesis
        phraseSet.map do |phrase, captured|
          left = phrase ? phrase.gsub(/\s*(<|>)\s*/, '\1') : nil
          right = captured ? captured.gsub(/\s*(<|>)\s*/, '\1')[1..-2] : nil
          [left, right]
        end
      end
    end
  end
end
