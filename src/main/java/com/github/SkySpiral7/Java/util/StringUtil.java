package com.github.SkySpiral7.Java.util;

import java.util.regex.Pattern;

public enum StringUtil {
    ;

    /**
     * @return the number of times that characterToFind occurs in stringToSearch
     */
    public static int countCharOccurrences(final String stringToSearch, final char characterToFind) {
        int count = 0;
        final int targetStringLength = stringToSearch.length();
        for (int i = 0; i < targetStringLength; ++i) {
            if (stringToSearch.charAt(i) == characterToFind) ++count;
        }
        return count;
        //same as: return Pattern.compile(""+characterToFind, Pattern.LITERAL).split(stringToSearch).length-1
        //this should be faster though since it doesn't need to create a substring with each match
    }

    /**
     * Code: return Pattern.compile(regexString).matcher(targetText).find();<br />
     * Reason: targetText.matches(regexString) auto adds both anchors
     */
    public static boolean regexFoundInString(final String targetText, final String regexString) {
        return Pattern.compile(regexString).matcher(targetText).find();
    }

    /**
     * The only reason to use this over the built in targetString.replaceFirst() is that this allows you to use flags.
     * entire code: return regex.matcher(targetString).replaceFirst(replacement);
     */
    public static String regexReplaceFirst(final String targetString, final Pattern regex, final String replacement) {
        return regex.matcher(targetString).replaceFirst(replacement);
    }

    /**
     * The only reason to use this over the built in targetString.replaceAll() is that this allows you to use flags.
     * entire code: return regex.matcher(targetString).replaceAll(replacement);
     */
    public static String regexReplaceAll(final String targetString, final Pattern regex, final String replacement) {
        return regex.matcher(targetString).replaceAll(replacement);
    }

    /**
     * This returns the same thing as Pattern.compile(searchingFor, Pattern.LITERAL).matcher(targetString).replaceFirst(replacement)
     * but this uses string indexOf to avoid the regex engine entirely and might be faster because of that
     */
    public static String literalReplaceFirst(final String targetString, final String searchingFor, final String replacement) {
        final int index = targetString.indexOf(searchingFor);
        if (index == -1) return targetString;  //not found
        final String firstPart = targetString.substring(0, index);
        final String secondPart = targetString.substring(index + searchingFor.length());
        return (firstPart + replacement + secondPart);
    }

    /**
     * Simply calls String.replace. This method only exists for orthogonality.
     */
    public static String literalReplaceAll(final String targetString, final String searchingFor, final String replacement) {
        return targetString.replace(searchingFor, replacement);
    }

    /**
     * The only reason to use this over the built in targetString.split() is that this allows you to use flags.
     * entire code: return regex.split(targetString);
     */
    public static String[] regexSplit(final String targetString, final Pattern regex) {
        return regex.split(targetString);
    }

    /**
     * This method allows you to split a String by a literal delimiter rather than by regex.
     * entire code: return Pattern.compile(delimiter, Pattern.LITERAL).split(targetString);
     */
    public static String[] literalSplit(final String targetString, final String delimiter) {
        return Pattern.compile(delimiter, Pattern.LITERAL).split(targetString);
    }

}
