#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <exception>
#include <string>
#include <vector>

#include <unicode/msgfmt.h>
#include <unicode/unistr.h>

extern "C" int harch_web_format_message(
    const char* locale_name,
    const char* pattern,
    const char* const* argument_names,
    const char* const* argument_texts,
    const int64_t* argument_numbers,
    const int* argument_kinds,
    int argument_count,
    char** result) {
  if (result == nullptr) return 1;
  *result = nullptr;
  if (argument_count < 0) return 1;
  try {
  UErrorCode status = U_ZERO_ERROR;
  icu::Locale locale(locale_name);
  icu::UnicodeString pattern_value = icu::UnicodeString::fromUTF8(pattern);
  icu::MessageFormat formatter(pattern_value, locale, status);
  if (U_FAILURE(status)) return 1;
  std::vector<icu::UnicodeString> names;
  std::vector<icu::Formattable> values;
  names.reserve(argument_count);
  values.reserve(argument_count);
  for (int index = 0; index < argument_count; ++index) {
    names.push_back(icu::UnicodeString::fromUTF8(argument_names[index]));
    if (argument_kinds[index] == 0) {
      values.emplace_back(icu::UnicodeString::fromUTF8(argument_texts[index]));
    } else {
      values.emplace_back(static_cast<int64_t>(argument_numbers[index]));
    }
  }
  icu::UnicodeString rendered;
  formatter.format(names.data(), values.data(), argument_count, rendered, status);
  if (U_FAILURE(status)) return 1;
  std::string utf8;
  rendered.toUTF8String(utf8);
  char* allocated = static_cast<char*>(std::malloc(utf8.size() + 1));
  if (allocated == nullptr) return 1;
  std::memcpy(allocated, utf8.c_str(), utf8.size() + 1);
  *result = allocated;
  return 0;
  } catch (const std::exception&) {
    return 1;
  } catch (...) {
    return 1;
  }
}
