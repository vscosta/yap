require "formula"

# Documentation: https://github.com/Homebrew/homebrew/wiki/Formula-Cookbook
#                /usr/local/Library/Contributions/example-formula.rb
# PLEASE REMOVE ALL GENERATED COMMENTS BEFORE SUBMITTING YOUR PULL REQUEST!

class Clamp < Formula
  homepage "https://bitbucket.org/multicoreware/cppamp-driver-ng/wiki/Home"
  version "0.0.1-3"
  url "https://bitbucket.org/multicoreware/cppamp-driver-ng/get/milestone3.tar.bz2"
  head "https://bitbucket.org/multicoreware/cppamp-driver-ng.git"
  sha1 "b8b88306561a60942f8ecbd8ff20554661c4e5f9"

  depends_on "cmake" => :build
  depends_on "wget" => :build
  depends_on "git" => :build
  depends_on "hg" => :build
  depends_on "subversion" => :build
  # depends_on :x11 # if your formula requires any X11/XQuartz components

  def install
    # ENV.deparallelize  # if your formula fails when building in parallel

    # Remove unrecognized options if warned by configure
    # system "./configure", "--disable-debug",
    #                      "--disable-dependency-tracking",
    #                      "--disable-silent-rules",
    #                      "--prefix=#{prefix}"
    mkdir "macbuild" do
      args = std_cmake_args
      args << "-DCLANG_URL=https://bitbucket.org/multicoreware/cppamp-ng.git"
      args << "-DLLVM_EXPERIMENTAL_TARGETS_TO_BUILD=CBackend"
      args << "-DGMAC_URL=https://bitbucket.org/multicoreware/gmac"
      system 'cmake', "..", *args
      system "make", "world"
      system "cd libc++; make install"
      system "make", "install" # if this fails, try separate make/make install steps
    end
  end

  test do
    # `test do` will create, run in and delete a temporary directory.
    #
    # This test will fail and we won't accept that! It's enough to just replace
    # "false" with the main program this formula installs, but it'd be nice if you
    # were more thorough. Run the test with `brew test milestone`.
    #
    # The installed folder is not in the path, so use the entire path to any
    # executables being tested: `system "#{bin}/program", "do", "something"`.
    system "make", "test"
  end
end
