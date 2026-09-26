#include <cstdint>
#include <cstddef>
#include <cstdlib>
#include <cstring>
#include <exception>
#include <limits>
#include <string>
#include <vector>

#include <unicode/locid.h>
#include <unicode/msgfmt.h>
#include <unicode/stringpiece.h>
#include <unicode/unistr.h>

extern "C" int harch_web_format_message(
    const char* locale_name,
    size_t locale_name_length,
    const char* pattern,
    size_t pattern_length,
    const char* const* argument_names,
    const size_t* argument_name_lengths,
    const char* const* argument_texts,
    const size_t* argument_text_lengths,
    const int64_t* argument_numbers,
    const int* argument_kinds,
    size_t argument_count,
    char** result,
    size_t* result_length) {
  if (result == nullptr || result_length == nullptr) return 1;
  *result = nullptr;
  *result_length = 0;
  if (locale_name == nullptr || pattern == nullptr ||
      locale_name_length > static_cast<size_t>(std::numeric_limits<int32_t>::max()) ||
      pattern_length > static_cast<size_t>(std::numeric_limits<int32_t>::max()) ||
      argument_count > static_cast<size_t>(std::numeric_limits<int32_t>::max()) ||
      (argument_count > 0 && (argument_names == nullptr || argument_name_lengths == nullptr ||
                              argument_texts == nullptr || argument_text_lengths == nullptr ||
                              argument_numbers == nullptr || argument_kinds == nullptr))) {
    return 1;
  }
  try {
  UErrorCode status = U_ZERO_ERROR;
  icu::Locale locale = icu::Locale::forLanguageTag(
      icu::StringPiece(locale_name, locale_name_length), status);
  if (U_FAILURE(status)) return 1;
  icu::UnicodeString pattern_value = icu::UnicodeString::fromUTF8(
      icu::StringPiece(pattern, pattern_length));
  icu::MessageFormat formatter(pattern_value, locale, status);
  if (U_FAILURE(status)) return 1;
  std::vector<icu::UnicodeString> names;
  std::vector<icu::Formattable> values;
  names.reserve(argument_count);
  values.reserve(argument_count);
  for (size_t index = 0; index < argument_count; ++index) {
    if (argument_names[index] == nullptr || argument_texts[index] == nullptr ||
        argument_name_lengths[index] > static_cast<size_t>(std::numeric_limits<int32_t>::max()) ||
        argument_text_lengths[index] > static_cast<size_t>(std::numeric_limits<int32_t>::max())) {
      return 1;
    }
    names.push_back(icu::UnicodeString::fromUTF8(
        icu::StringPiece(argument_names[index], argument_name_lengths[index])));
    if (argument_kinds[index] == 0) {
      values.emplace_back(icu::UnicodeString::fromUTF8(
          icu::StringPiece(argument_texts[index], argument_text_lengths[index])));
    } else {
      values.emplace_back(static_cast<int64_t>(argument_numbers[index]));
    }
  }
  icu::UnicodeString rendered;
  formatter.format(names.data(), values.data(), static_cast<int32_t>(argument_count), rendered, status);
  if (U_FAILURE(status)) return 1;
  std::string utf8;
  rendered.toUTF8String(utf8);
  if (utf8.size() > std::numeric_limits<size_t>::max() - 1) return 1;
  char* allocated = static_cast<char*>(std::malloc(utf8.size() + 1));
  if (allocated == nullptr) return 1;
  std::memcpy(allocated, utf8.data(), utf8.size());
  allocated[utf8.size()] = '\0';
  *result = allocated;
  *result_length = utf8.size();
  return 0;
  } catch (const std::exception&) {
    return 1;
  } catch (...) {
    return 1;
  }
}
