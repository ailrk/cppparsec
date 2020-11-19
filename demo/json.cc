#include "json.h"
#include <array>
#include <sstream>

// test json parser. note the parser is defined at `json.h`

std::string
get_dir(const std::string &filepath) {
    auto s = filepath.begin();
    for (auto iter = filepath.rbegin(); iter != filepath.rend(); ++iter) {
        if (*iter == '/') {
            return std::string{ s, iter.base() };
        }
    }
    return filepath;
}

constexpr std::array<const char *, 8> testfiles = {
    "glossary.json", "menu.json",     " simple-array.json", "simple1.json",
    "simple2.json",  " simple3.json", "simple4.json",       "widget.json"
};

std::vector<std::string>
get_paths() {
    std::vector<std::string> paths{};
    std::string path = get_dir(__FILE__); // + "/demo1.json";
    for (auto &name : testfiles) {
        path += "/jsons/";
        path += name;
    }
    return paths;
}

void
test(parser<string_state, jvalue_t> &jvalue, const std::string &path) {
    // runner
    std::ifstream ifs(path);
    std::string demo{ std::istreambuf_iterator<char>(ifs),
                      std::istreambuf_iterator<char>() };

    std::cout << path << std::endl;
    std::cout << demo << std::endl;
    std::cout << "==============" << std::endl;
    jvalue_t v = jvalue(string_state(demo)).get();
}

void
test_all() {
    auto jvalue = json_parser();
    auto paths = get_paths();
    for (auto &path : paths) {
        test(jvalue, path);
    }
}

// test basic json elements like null and numbers.
void
test_basic() {
    auto jvalue = json_parser();
    std::string dir = get_dir(__FILE__);
    std::string path = dir + "/jsons/basic";
    std::ifstream ifs(path);
    auto iter = std::istreambuf_iterator<char>(ifs);
    auto end = std::istreambuf_iterator<char>();

    std::cout << "Tessing basic json components" << std::endl;
    while (iter != end) {

        std::stringstream ss;
        std::string data;

        char type = *iter;
        iter++;

        std::cout << "type: " << type << std::endl;
        do {
            ss << *iter;
            iter++;
        } while (*iter != '\n');
        iter++;

        data = ss.str();
        std::cout << "- testing " << data << "\n";

        jvalue_t v = jvalue(string_state(data)).get();
        switch (type) {
        case 's': {
            auto s = std::get<std::string>(v);
            std::cout << s << "\n";
        } break;
        case 'N': {
            auto s = std::get<jnull_t>(v);
            assert(sizeof(s) == 1);
            std::cout << "null"
                      << "\n";
        } break;
        case 'n': {
            auto s = std::get<int>(v);
            std::cout << s << "\n";
        } break;
        case 'b': {
            auto s = std::get<int>(v);
            std::cout << (s ? "true" : "false") << "\n";
        } break;
        case 'a': {
            auto s = std::get<std::shared_ptr<jarray_t>>(v);
            std::cout << "[array]" << std::endl;
        } break;
        case 'o': {
            auto s = std::get<std::shared_ptr<jobject_t>>(v);
            std::cout << "[object]" << std::endl;
        } break;
        }
    }
}

int
main(void) {
    test_basic();

#ifdef TEST_ALL
    test_all();
#endif

    return 0;
}
